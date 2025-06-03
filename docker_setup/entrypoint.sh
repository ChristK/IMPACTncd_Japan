#!/bin/bash
# -----------------------------------------------------------------------------
# entrypoint.sh - Docker Container User Identity Management
# -----------------------------------------------------------------------------
#
# PURPOSE:
# This entrypoint script handles dynamic user creation for non-root Docker execution.
# It allows the container to run with the same user ID and group ID as the host user,
# which is crucial for avoiding file permission issues when mounting directories
# between the host and container.
#
# HOW IT WORKS:
# 1. Gets user information from environment variables:
#    - USER_ID and GROUP_ID: the numeric IDs from the host
#    - USER_NAME and GROUP_NAME: the actual username and group name from the host
#    - Falls back to defaults (dockeruser/dockergroup) if not provided
#
# 2. Handles root execution:
#    - If no USER_ID is specified or if running as root (UID 0), executes command directly
#
# 3. Creates group if needed:
#    - Checks if a group with the specified GROUP_ID exists
#    - If not, creates a new group with the name GROUP_NAME and ID GROUP_ID
#
# 4. Creates user if needed:
#    - Checks if a user with the specified USER_ID exists
#    - If not, creates a new user with:
#      * Username: USER_NAME
#      * UID: USER_ID
#      * Primary group: GROUP_ID
#      * Home directory: /home/$USER_NAME
#      * Shell: /bin/bash
#    - Ensures the home directory has correct ownership
#
# 5. Executes the command as the specified user:
#    - Uses 'gosu' to switch to the non-root user and execute the original command
#
# WHY THIS MATTERS:
# Without this script, the container would run as root or a generic user, causing
# files created inside the container to have incorrect ownership when viewed from
# the host system. This script ensures that files created in the container have
# the same ownership as if they were created directly on the host by the actual user.
#
# This is particularly important for this project since directories are mounted
# between the host and container, and output files should be owned by the actual
# user account rather than root or a generic dockeruser.
#
# USAGE:
# This script is automatically called as the Docker ENTRYPOINT when the container
# starts. The create_env.sh script passes the necessary environment variables:
#   -e USER_ID="$(id -u)"
#   -e GROUP_ID="$(id -g)"
#   -e USER_NAME="$(whoami)"
#   -e GROUP_NAME="$(id -gn)"
#
# -----------------------------------------------------------------------------

# Get the user and group IDs from environment variables passed by Docker
USER_ID=${USER_ID:-$(id -u)}
GROUP_ID=${GROUP_ID:-$(id -g)}
USER_NAME=${USER_NAME:-"dockeruser"}
GROUP_NAME=${GROUP_NAME:-"dockergroup"}

# If no specific user is requested or running as root, just execute the command
if [ -z "$USER_ID" ] || [ "$USER_ID" = "0" ]; then
    exec "$@"
fi

# Create group if it doesn't exist
if ! getent group "$GROUP_ID" > /dev/null 2>&1; then
    groupadd -g "$GROUP_ID" "$GROUP_NAME"
fi

# Create user if it doesn't exist
if ! getent passwd "$USER_ID" > /dev/null 2>&1; then
    # Create user with home directory
    useradd -u "$USER_ID" -g "$GROUP_ID" -m -s /bin/bash "$USER_NAME"
    
    # Ensure home directory has correct ownership
    chown "$USER_ID:$GROUP_ID" "/home/$USER_NAME"
fi

# Execute the command as the specified user
exec gosu "$USER_ID:$GROUP_ID" "$@"
