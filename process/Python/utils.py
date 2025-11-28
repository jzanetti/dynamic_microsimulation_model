import logging

# Configure the root logger
logging.basicConfig(
    level=logging.DEBUG,  # Set the minimum level to log (DEBUG logs everything)
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',  # Custom format
    handlers=[
        logging.StreamHandler()  # Output to console
    ]
)