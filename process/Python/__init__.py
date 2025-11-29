from logging import basicConfig, DEBUG, StreamHandler

basicConfig(
    level=DEBUG,  # Set the minimum level to log (DEBUG logs everything)
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",  # Custom format
    handlers=[StreamHandler()],  # Output to console
)

# <><><><><><><><><><><><><><><><><><><><><><><><><>
# SAMPLE DATA
# <><><><><><><><><><><><><><><><><><><><><><><><><>
