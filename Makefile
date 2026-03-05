.PHONY: build clean rebuild test help

# Default target
all: build

# Build the library
build:
	lake build

# Clean build artifacts
clean:
	lake clean

# Clean and rebuild from scratch
rebuild: clean build

# Build and run tests
test:
	lake build tests
	.lake/build/bin/tests

# Show help
help:
	@echo "Available targets:"
	@echo "  build   - Build the library"
	@echo "  clean   - Clean build artifacts"
	@echo "  rebuild - Clean and rebuild"
	@echo "  test    - Build and run tests"
	@echo "  help    - Show this help"
