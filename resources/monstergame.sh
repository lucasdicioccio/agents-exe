#!/bin/bash
# =============================================================================
# MonsterGame Database Setup Script
# -----------------------------------------------------------------------------
# A helper script for provisioning and migrating the MonsterGame PostgreSQL
# database. Designed to work with PostgREST for API access.
#
# Usage:
#   ./monstergame.sh provision  # Create user, database, and grant permissions
#   ./monstergame.sh migrate    # Run the SQL migration script
#
# Environment Variables:
#   DB_USER       - Database username (default: monstergame)
#   DB_NAME       - Database name (default: monstergame)
#   DB_PASSWORD   - Database password (default: monstergame123)
#   DB_HOST       - Database host (default: 127.0.0.1)
#   DB_PORT       - Database port (default: 5432)
#   PSQL          - PostgreSQL client command (default: psql)
#   CREATEDB      - Createdb command (default: createdb)
#   CREATEUSER    - Createuser command (default: createuser)
#
# Requirements:
#   - PostgreSQL client tools (psql, createdb, createuser)
#   - Superuser access for provisioning
# =============================================================================

set -euo pipefail
set -x

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
DB_USER="${DB_USER:-monstergame}"
DB_NAME="${DB_NAME:-monstergame}"
DB_PASSWORD="${DB_PASSWORD:-monstergame123}"
DB_HOST="${DB_HOST:-127.0.0.1}"
DB_PORT="${DB_PORT:-5432}"

PSQL="${PSQL:-psql}"
CREATEDB="${CREATEDB:-createdb}"
CREATEUSER="${CREATEUSER:-createuser}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL_FILE="${SCRIPT_DIR}/monstergame.sql"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_postgres() {
    if ! command -v "$PSQL" &> /dev/null; then
        log_error "PostgreSQL client (psql) not found. Please install PostgreSQL client tools."
        exit 1
    fi
    
    if ! $PSQL -h "$DB_HOST" -p "$DB_PORT" -c "SELECT 1;" postgres &> /dev/null; then
        log_error "Cannot connect to PostgreSQL at ${DB_HOST}:${DB_PORT}"
        log_error "Make sure PostgreSQL is running and accessible."
        exit 1
    fi
    
    log_info "PostgreSQL connection verified at ${DB_HOST}:${DB_PORT}"
}

check_sql_file() {
    if [[ ! -f "$SQL_FILE" ]]; then
        log_error "SQL migration file not found: $SQL_FILE"
        exit 1
    fi
}

# -----------------------------------------------------------------------------
# Provision Command
# -----------------------------------------------------------------------------
cmd_provision() {
    log_info "Starting database provisioning..."
    log_info "Configuration:"
    log_info "  Database User: $DB_USER"
    log_info "  Database Name: $DB_NAME"
    log_info "  Host: $DB_HOST:$DB_PORT"
    
    check_postgres
    
    # Check if we have superuser privileges
    if ! $PSQL -h "$DB_HOST" -p "$DB_PORT" -c "SELECT 1;" postgres &> /dev/null; then
        log_error "Superuser access required for provisioning."
        log_error "Please run with a user that has CREATEUSER and CREATEDB privileges."
        exit 1
    fi
    
    # Create user if it doesn't exist
    log_info "Checking for database user '$DB_USER'..."
    if $PSQL -h "$DB_HOST" -p "$DB_PORT" -tAc "SELECT 1 FROM pg_roles WHERE rolname='$DB_USER';" postgres | grep -q 1; then
        log_warning "User '$DB_USER' already exists, skipping creation."
    else
        log_info "Creating database user '$DB_USER'..."
        if command -v "$CREATEUSER" &> /dev/null; then
            $CREATEUSER -h "$DB_HOST" -p "$DB_PORT" -P "$DB_USER" <<EOF
$DB_PASSWORD
$DB_PASSWORD
EOF
        else
            $PSQL -h "$DB_HOST" -p "$DB_PORT" postgres -c "CREATE USER \"$DB_USER\" WITH PASSWORD '$DB_PASSWORD';"
        fi
        log_success "User '$DB_USER' created."
    fi
    
    # Create database if it doesn't exist
    log_info "Checking for database '$DB_NAME'..."
    if $PSQL -h "$DB_HOST" -p "$DB_PORT" -tAc "SELECT 1 FROM pg_database WHERE datname='$DB_NAME';" postgres | grep -q 1; then
        log_warning "Database '$DB_NAME' already exists, skipping creation."
    else
        log_info "Creating database '$DB_NAME'..."
        if command -v "$CREATEDB" &> /dev/null; then
            $CREATEDB -h "$DB_HOST" -p "$DB_PORT" -O "$DB_USER" "$DB_NAME"
        else
            $PSQL -h "$DB_HOST" -p "$DB_PORT" postgres -c "CREATE DATABASE \"$DB_NAME\" OWNER \"$DB_USER\";"
        fi
        log_success "Database '$DB_NAME' created with owner '$DB_USER'."
    fi
    
    # Grant privileges
    log_info "Granting privileges to '$DB_USER'..."
    $PSQL -h "$DB_HOST" -p "$DB_PORT" "$DB_NAME" -c "
        GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO \"$DB_USER\";
        GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO \"$DB_USER\";
        ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO \"$DB_USER\";
        ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON SEQUENCES TO \"$DB_USER\";
    " 2>/dev/null || true
    
    log_success "Database provisioning complete!"
    log_info ""
    log_info "Connection details:"
    log_info "  Host:     $DB_HOST"
    log_info "  Port:     $DB_PORT"
    log_info "  Database: $DB_NAME"
    log_info "  User:     $DB_USER"
    log_info "  Password: $DB_PASSWORD"
    log_info ""
    log_info "PostgREST connection string:"
    log_info "  postgres://$DB_USER:$DB_PASSWORD@$DB_HOST:$DB_PORT/$DB_NAME"
}

# -----------------------------------------------------------------------------
# Migrate Command
# -----------------------------------------------------------------------------
cmd_migrate() {
    log_info "Starting database migration..."
    
    check_postgres
    check_sql_file
    
    # Check if database exists
    if ! $PSQL -h "$DB_HOST" -p "$DB_PORT" -tAc "SELECT 1 FROM pg_database WHERE datname='$DB_NAME';" postgres | grep -q 1; then
        log_error "Database '$DB_NAME' does not exist."
        log_error "Please run: ./monstergame.sh provision"
        exit 1
    fi
    
    log_info "Running migration from: $SQL_FILE"
    log_info "Target database: $DB_NAME"
    log_info "This may take a moment..."
    log_info ""
    
    # Run the migration
    if $PSQL -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -v ON_ERROR_STOP=1 -f "$SQL_FILE"; then
        log_success "Migration completed successfully!"
        log_info ""
        log_info "Database '$DB_NAME' now contains:"
        
        # Show summary of created objects
        local tables
        tables=$($PSQL -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -tAc "
            SELECT COUNT(*) FROM information_schema.tables 
            WHERE table_schema = 'public' AND table_type = 'BASE TABLE';
        " 2>/dev/null || echo "0")
        
        local views
        views=$($PSQL -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -tAc "
            SELECT COUNT(*) FROM information_schema.views 
            WHERE table_schema = 'public';
        " 2>/dev/null || echo "0")
        
        local functions
        functions=$($PSQL -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -tAc "
            SELECT COUNT(*) FROM pg_proc p 
            JOIN pg_namespace n ON p.pronamespace = n.oid 
            WHERE n.nspname = 'public';
        " 2>/dev/null || echo "0")
        
        log_info "  Tables:    $tables"
        log_info "  Views:     $views"
        log_info "  Functions: $functions"
        log_info ""
        log_info "Sample queries to try:"
        log_info "  $PSQL -h $DB_HOST -p $DB_PORT -d $DB_NAME -c \"SELECT * FROM trainers;\""
        log_info "  $PSQL -h $DB_HOST -p $DB_PORT -d $DB_NAME -c \"SELECT * FROM trainer_statistics;\""
    else
        log_error "Migration failed!"
        exit 1
    fi
}

# -----------------------------------------------------------------------------
# Status Command (Bonus)
# -----------------------------------------------------------------------------
cmd_status() {
    log_info "Checking database status..."
    
    if ! command -v "$PSQL" &> /dev/null; then
        log_error "PostgreSQL client not found."
        exit 1
    fi
    
    if ! $PSQL -h "$DB_HOST" -p "$DB_PORT" -c "SELECT 1;" postgres &> /dev/null; then
        log_error "Cannot connect to PostgreSQL at ${DB_HOST}:${DB_PORT}"
        exit 1
    fi
    
    log_success "PostgreSQL is running at ${DB_HOST}:${DB_PORT}"
    
    # Check if database exists
    if $PSQL -h "$DB_HOST" -p "$DB_PORT" -tAc "SELECT 1 FROM pg_database WHERE datname='$DB_NAME';" postgres | grep -q 1; then
        log_success "Database '$DB_NAME' exists"
        
        # Show table counts
        echo ""
        $PSQL -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -c "
            SELECT 
                'Trainers' as entity, 
                COUNT(*)::text as count 
            FROM trainers
            UNION ALL
            SELECT 'Monster Species', COUNT(*)::text FROM monster_species
            UNION ALL
            SELECT 'Monsters', COUNT(*)::text FROM monsters
            UNION ALL
            SELECT 'Moves', COUNT(*)::text FROM moves
            UNION ALL
            SELECT 'Items', COUNT(*)::text FROM items
            UNION ALL
            SELECT 'Battles', COUNT(*)::text FROM battles;
        " 2>/dev/null || log_warning "Could not query table counts (migration may not have run yet)"
    else
        log_warning "Database '$DB_NAME' does not exist"
        log_info "Run: ./monstergame.sh provision"
    fi
}

# -----------------------------------------------------------------------------
# Reset Command (Bonus - Dangerous!)
# -----------------------------------------------------------------------------
cmd_reset() {
    log_warning "WARNING: This will DELETE the database '$DB_NAME' and all its data!"
    read -p "Are you sure? Type 'yes' to continue: " confirm
    
    if [[ "$confirm" != "yes" ]]; then
        log_info "Reset cancelled."
        exit 0
    fi
    
    log_info "Dropping database '$DB_NAME'..."
    $PSQL -h "$DB_HOST" -p "$DB_PORT" postgres -c "DROP DATABASE IF EXISTS \"$DB_NAME\";" 2>/dev/null || true
    
    log_info "Dropping user '$DB_USER'..."
    $PSQL -h "$DB_HOST" -p "$DB_PORT" postgres -c "DROP USER IF EXISTS \"$DB_USER\";" 2>/dev/null || true
    
    log_success "Database and user dropped."
    log_info "Run './monstergame.sh provision' to recreate."
}

# -----------------------------------------------------------------------------
# Help Command
# -----------------------------------------------------------------------------
cmd_help() {
    cat <<EOF
MonsterGame Database Management Script

Usage: ./monstergame.sh <command>

Commands:
  provision    Create the database user and database
  migrate      Run the SQL migration script (creates tables, views, sample data)
  status       Check database connection and show table counts
  reset        DANGER: Drop database and user (requires confirmation)
  help         Show this help message

Environment Variables:
  DB_USER       Database username (default: monstergame)
  DB_NAME       Database name (default: monstergame)
  DB_PASSWORD   Database password (default: monstergame123)
  DB_HOST       Database host (default: 127.0.0.1)
  DB_PORT       Database port (default: 5432)

Examples:
  # Use default settings
  ./monstergame.sh provision
  ./monstergame.sh migrate

  # Use custom database name and credentials
  DB_NAME=mygame DB_USER=myuser DB_PASSWORD=secret ./monstergame.sh provision

  # Connect to remote PostgreSQL
  DB_HOST=db.example.com DB_PORT=5433 ./monstergame.sh migrate

PostgREST Setup:
  After running provision and migrate, you can start PostgREST with:
    postgrest -c config.conf

  Or using Docker:
    docker run -p 3000:3000 \\
      -e PGRST_DB_URI="postgres://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_NAME}" \\
      -e PGRST_DB_SCHEMA="public" \\
      -e PGRST_DB_ANON_ROLE="${DB_USER}" \\
      postgrest/postgrest

EOF
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
main() {
    local command="${1:-help}"
    
    case "$command" in
        provision)
            cmd_provision
            ;;
        migrate)
            cmd_migrate
            ;;
        status)
            cmd_status
            ;;
        reset)
            cmd_reset
            ;;
        help|--help|-h)
            cmd_help
            ;;
        *)
            log_error "Unknown command: $command"
            echo ""
            cmd_help
            exit 1
            ;;
    esac
}

main "$@"

