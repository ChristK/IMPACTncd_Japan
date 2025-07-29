#!/bin/bash

#===============================================================================
# MEMORY PROFILER FOR R SCRIPTS
#===============================================================================
# Author: IMPACTncd Japan Team
# Purpose: Monitor memory usage (RSS and VSZ) of R scripts during execution
# 
# This script runs an R script while continuously monitoring its memory usage
# and generates both a log file and a visualization plot of memory consumption
# over time.
#
# USAGE:
#   ./mem_profile.sh <R_script> <output_log_file>
#
# ARGUMENTS:
#   R_script        - Path to the R script to profile
#   output_log_file - Path where memory log will be saved (*.txt)
#
# OUTPUTS:
#   1. Memory log file (*.txt) - Timestamped memory usage data
#   2. Memory plot (*.png) - Visual representation of memory usage over time
#
# MEMORY METRICS:
#   RSS (Resident Set Size) - Physical memory currently used by the process
#   VSZ (Virtual Memory Size) - Total virtual memory used by the process
#   MEM% - Percentage of system memory used by the process
#
# EXAMPLES:
#   ./mem_profile.sh simulate.R memory_log.txt
#   ./mem_profile.sh calibrate.R /tmp/calib_memory.txt
#
# NOTES:
#   - Sampling interval: 0.1 seconds (100ms)
#   - Memory values are reported in GB for readability
#   - Script can be interrupted with Ctrl+C safely
#   - Requires R and basic Unix utilities (ps, awk, date)
#===============================================================================

# Usage check with detailed help
if [ "$#" -ne 2 ]; then
  echo "ERROR: Incorrect number of arguments"
  echo ""
  echo "USAGE: $0 <R_script> <output_log_file>"
  echo ""
  echo "ARGUMENTS:"
  echo "  R_script        Path to the R script to profile"
  echo "  output_log_file Path for memory log output (*.txt)"
  echo ""
  echo "EXAMPLE:"
  echo "  $0 simulate.R memory_usage.txt"
  echo ""
  echo "OUTPUT FILES:"
  echo "  - <output_log_file>     : Timestamped memory usage data"
  echo "  - <output_log_file>.png : Memory usage visualization"
  exit 1
fi

# Parse command line arguments
SCRIPT="$1"
MEMLOG="$2"
PLOTFILE="${MEMLOG%.txt}.png"

# Validate input files and paths
if [ ! -f "$SCRIPT" ]; then
  echo "ERROR: R script '$SCRIPT' not found"
  exit 1
fi

if [ ! -d "$(dirname "$MEMLOG")" ]; then
  echo "ERROR: Directory '$(dirname "$MEMLOG")' does not exist"
  exit 1
fi

echo "==============================================================================="
echo "MEMORY PROFILER STARTING"
echo "==============================================================================="
echo "R Script:    $SCRIPT"
echo "Memory Log:  $MEMLOG"
echo "Plot File:   $PLOTFILE"
echo "Start Time:  $(date)"
echo "==============================================================================="

# Record start time for duration calculation
START_TIME=$(date +%s.%N)

# Start R script in background and capture its PID
echo "Starting R script..."
Rscript "$SCRIPT" &
R_PID=$!

# Set up signal handler for graceful interruption
trap "kill $R_PID 2>/dev/null; echo ''; echo 'INTERRUPTED: Cleaning up...'; exit 1" INT TERM

# Initialize peak memory trackers
max_rss_kb=0  # Peak RSS in KB
max_vsz_kb=0  # Peak VSZ in KB

# Create memory log file with header
echo "# Memory Profile Log for: $SCRIPT" > "$MEMLOG"
echo "# Generated on: $(date)" >> "$MEMLOG"
echo "# Format: Timestamp RSS_GB VSZ_GB MEM_PCT" >> "$MEMLOG"
echo "# RSS_GB: Resident Set Size in Gigabytes" >> "$MEMLOG"
echo "# VSZ_GB: Virtual Memory Size in Gigabytes" >> "$MEMLOG"
echo "# MEM_PCT: Percentage of system memory used" >> "$MEMLOG"

echo "Monitoring memory usage (sampling every 0.1 seconds)..."
sample_count=0

# Main monitoring loop - continues while R process is running
while ps -p $R_PID > /dev/null; do
  # Get current timestamp
  ts=$(date +"%H:%M:%S.%3N")
  
  # Get memory information for the R process
  # RSS: Physical memory currently used (KB)
  # VSZ: Virtual memory size (KB)
  # %MEM: Percentage of system memory used
  mem_info=$(ps -o rss,vsz,%mem -p $R_PID | tail -n 1)
  
  # Parse memory values
  rss_kb=$(echo $mem_info | awk '{print $1}')
  vsz_kb=$(echo $mem_info | awk '{print $2}')
  mem_pct=$(echo $mem_info | awk '{print $3}')

  # Update peak memory trackers
  [ "$rss_kb" -gt "$max_rss_kb" ] && max_rss_kb=$rss_kb
  [ "$vsz_kb" -gt "$max_vsz_kb" ] && max_vsz_kb=$vsz_kb

  # Convert KB to GB for better readability
  rss_gb=$(awk "BEGIN {printf \"%.3f\", $rss_kb/1024/1024}")
  vsz_gb=$(awk "BEGIN {printf \"%.3f\", $vsz_kb/1024/1024}")

  # Write to log file
  echo "$ts $rss_gb $vsz_gb $mem_pct" >> "$MEMLOG"
  
  # Progress indicator (every 50 samples = 5 seconds)
  sample_count=$((sample_count + 1))
  if [ $((sample_count % 50)) -eq 0 ]; then
    echo "  Sample $sample_count: RSS=${rss_gb}GB, VSZ=${vsz_gb}GB, MEM%=${mem_pct}%"
  fi

  # Wait before next sample
  sleep 0.1
done

# Wait for R script to complete and calculate execution time
wait $R_PID
R_EXIT_CODE=$?
END_TIME=$(date +%s.%N)
DURATION=$(awk "BEGIN {printf \"%.2f\", $END_TIME - $START_TIME}")

# Convert peak memory values to GB
MAX_RSS_GB=$(awk "BEGIN {printf \"%.3f\", $max_rss_kb/1024/1024}")
MAX_VSZ_GB=$(awk "BEGIN {printf \"%.3f\", $max_vsz_kb/1024/1024}")

# Print execution summary
echo ""
echo "==============================================================================="
echo "MEMORY PROFILING COMPLETED"
echo "==============================================================================="
echo "R Script:      $SCRIPT"
echo "Exit Code:     $R_EXIT_CODE"
echo "Duration:      ${DURATION}s"
echo "Total Samples: $sample_count"
echo ""
echo "PEAK MEMORY USAGE:"
echo "  Peak RSS:    ${MAX_RSS_GB} GB  (Physical memory)"
echo "  Peak VSZ:    ${MAX_VSZ_GB} GB  (Virtual memory)"
echo ""
echo "OUTPUT FILES:"
echo "  Memory Log:  $MEMLOG"
echo "  Plot File:   $PLOTFILE"
echo "==============================================================================="

# Generate memory usage visualization plot
echo ""
echo "Generating memory usage plot..."

# Create plot using R
Rscript - <<EOF
# Read memory log data
log <- read.table("$MEMLOG", header = FALSE, comment.char = "#")
colnames(log) <- c("Time", "RSS_GB", "VSZ_GB", "MEM_PCT")
log\$Index <- seq_len(nrow(log))

# Create PNG plot
png("$PLOTFILE", width = 1000, height = 600, res = 100)

# Set up plotting parameters for better appearance
par(mar = c(5, 4, 4, 2) + 0.1, cex.main = 1.2, cex.lab = 1.1)

# Create the main plot
plot(log\$Index, log\$RSS_GB, type = "l", col = "blue", lwd = 2,
     xlab = "Time (samples at 0.1 sec intervals)", 
     ylab = "Memory Usage (GB)",
     main = paste("Memory Profile:", basename("$SCRIPT")),
     ylim = range(c(log\$RSS_GB, log\$VSZ_GB)))

# Add VSZ line
lines(log\$Index, log\$VSZ_GB, col = "red", lwd = 2)

# Add grid for better readability
grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")

# Add legend with additional information
legend("topright", 
       legend = c(paste("RSS (Physical) - Peak:", sprintf("%.3f GB", max(log\$RSS_GB))),
                  paste("VSZ (Virtual) - Peak:", sprintf("%.3f GB", max(log\$VSZ_GB)))),
       col = c("blue", "red"), 
       lwd = 2,
       cex = 0.9)

# Add summary text
mtext(paste("Duration:", "$DURATION", "seconds | Samples:", nrow(log)), 
      side = 1, line = 4, cex = 0.8, col = "gray50")

dev.off()

# Print plot statistics
cat("Plot generated successfully:\\n")
cat("  File:", "$PLOTFILE", "\\n")
cat("  Dimensions: 1000x600 pixels\\n")
cat("  Data points:", nrow(log), "\\n")
EOF

# Optional: Display plot file information
if [ -f "$PLOTFILE" ]; then
  echo ""
  echo "PLOT GENERATION SUCCESSFUL"
  echo "Plot saved to: $PLOTFILE"
  echo "File size: $(du -h "$PLOTFILE" | cut -f1)"
else
  echo ""
  echo "WARNING: Plot generation may have failed"
  echo "Check R installation and plotting capabilities"
fi

echo ""
echo "==============================================================================="
echo "MEMORY PROFILING SESSION COMPLETE"
echo "==============================================================================="

# Exit with the same code as the R script
exit $R_EXIT_CODE

# Optional plot viewing (commented out as it doesn't work over SSH)
# Uncomment these lines if running locally with GUI support:
# if command -v xdg-open &> /dev/null; then
#   echo "Opening plot with default viewer..."
#   xdg-open "$PLOTFILE" &> /dev/null &
# elif command -v open &> /dev/null; then  # for macOS
#   echo "Opening plot with default viewer..."
#   open "$PLOTFILE" &> /dev/null &
# fi