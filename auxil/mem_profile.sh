#!/bin/bash

# Usage check
if [ "$#" -ne 2 ]; then
  echo "Usage: $0 myRscript.R /path/to/memlog.txt"
  exit 1
fi

SCRIPT="$1"
MEMLOG="$2"
PLOTFILE="${MEMLOG%.txt}.png"

START_TIME=$(date +%s.%N)
Rscript "$SCRIPT" &
R_PID=$!

trap "kill $R_PID 2>/dev/null; echo 'Interrupted'; exit 1" INT TERM

# Initialize peak trackers
max_rss_kb=0
max_vsz_kb=0

# Create log file
echo "# Memory log: $SCRIPT" > "$MEMLOG"

while ps -p $R_PID > /dev/null; do
  ts=$(date +"%H:%M:%S.%3N")
  mem_info=$(ps -o rss,vsz,%mem -p $R_PID | tail -n 1)
  
  rss_kb=$(echo $mem_info | awk '{print $1}')
  vsz_kb=$(echo $mem_info | awk '{print $2}')
  mem_pct=$(echo $mem_info | awk '{print $3}')

  [ "$rss_kb" -gt "$max_rss_kb" ] && max_rss_kb=$rss_kb
  [ "$vsz_kb" -gt "$max_vsz_kb" ] && max_vsz_kb=$vsz_kb

  rss_gb=$(awk "BEGIN {printf \"%.3f\", $rss_kb/1024/1024}")
  vsz_gb=$(awk "BEGIN {printf \"%.3f\", $vsz_kb/1024/1024}")

  echo "$ts $rss_gb $vsz_gb $mem_pct" >> "$MEMLOG"

  sleep 0.1
done

wait $R_PID
END_TIME=$(date +%s.%N)
DURATION=$(awk "BEGIN {printf \"%.2f\", $END_TIME - $START_TIME}")
MAX_RSS_GB=$(awk "BEGIN {printf \"%.3f\", $max_rss_kb/1024/1024}")
MAX_VSZ_GB=$(awk "BEGIN {printf \"%.3f\", $max_vsz_kb/1024/1024}")

# Print summary
echo "-----------------------------------------"
echo "R script completed: $SCRIPT"
echo "Duration:     $DURATION seconds"
echo "Peak RSS:     $MAX_RSS_GB GB"
echo "Peak VSZ:     $MAX_VSZ_GB GB"
echo "Memory log:   $MEMLOG"
echo "Plot output:  $PLOTFILE"
echo "-----------------------------------------"

# Create plot in R
Rscript - <<EOF
log <- read.table("$MEMLOG", header = FALSE, comment.char = "#")
colnames(log) <- c("Time", "RSS_GB", "VSZ_GB", "MEM_PCT")
log\$Index <- seq_len(nrow(log))

png("$PLOTFILE", width = 800, height = 500)
plot(log\$Index, log\$RSS_GB, type = "l", col = "blue", lwd = 2,
     xlab = "Time (0.1 sec intervals)", ylab = "Memory Usage (GB)",
     main = "Memory Profile: $SCRIPT", ylim = range(c(log\$RSS_GB, log\$VSZ_GB)))
lines(log\$Index, log\$VSZ_GB, col = "red", lwd = 2)
legend("topright", legend = c("RSS (GB)", "VSZ (GB)"),
       col = c("blue", "red"), lwd = 2)
dev.off()
EOF

# Attempt to open plot if on desktop (not working over ssh)
# if command -v xdg-open &> /dev/null; then
#   xdg-open "$PLOTFILE" &> /dev/null &
# elif command -v open &> /dev/null; then  # for macOS
#   open "$PLOTFILE" &> /dev/null &
# fi