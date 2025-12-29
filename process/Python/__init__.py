from logging import basicConfig, DEBUG, StreamHandler

basicConfig(
    level=DEBUG,  # Set the minimum level to log (DEBUG logs everything)
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",  # Custom format
    handlers=[StreamHandler()],  # Output to console
)


TEST_RUN = False

RUF_METHOD = None # top30 or None


RUN_LOG = True