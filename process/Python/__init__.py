from logging import basicConfig, DEBUG, StreamHandler

basicConfig(
    level=DEBUG,  # Set the minimum level to log (DEBUG logs everything)
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',  # Custom format
    handlers=[
        StreamHandler()  # Output to console
    ]
)

# <><><><><><><><><><><><><><><><><><><><><><><><><>
# SAMPLE DATA
# <><><><><><><><><><><><><><><><><><><><><><><><><>
SAMPLE_DATA_DIR = "etc/data/"
SAMPLE_DATA_CFG = {
    "household_types": [
        # --- Single Person ---
        {'code': '1A',      'A': 1, 'S': 0, 'C': 0, 'prob': 0.14}, # Single Worker
        {'code': '1S',      'A': 0, 'S': 1, 'C': 0, 'prob': 0.10}, # Single Retiree
        # --- Couples (No Kids) ---
        {'code': '2A',      'A': 2, 'S': 0, 'C': 0, 'prob': 0.15}, # DINKs / Pre-kids
        {'code': '1A+1S',   'A': 1, 'S': 1, 'C': 0, 'prob': 0.03}, # Bridging retirement
        {'code': '2S',      'A': 0, 'S': 2, 'C': 0, 'prob': 0.08}, # Retired Couple
        # --- Families with Children ---
        {'code': '2A+1C',   'A': 2, 'S': 0, 'C': 1, 'prob': 0.10},
        {'code': '2A+2C',   'A': 2, 'S': 0, 'C': 2, 'prob': 0.12},
        {'code': '2A+3C',   'A': 2, 'S': 0, 'C': 3, 'prob': 0.05},
        {'code': '1A+1C',   'A': 1, 'S': 0, 'C': 1, 'prob': 0.05}, # Solo Parent 1 kid
        {'code': '1A+2C',   'A': 1, 'S': 0, 'C': 2, 'prob': 0.04}, # Solo Parent 2 kids
        # --- Multi-Generational / Flatting ---
        {'code': '3A',      'A': 3, 'S': 0, 'C': 0, 'prob': 0.05}, # Flatting/Adult Kids at home
        {'code': '2A+1S',   'A': 2, 'S': 1, 'C': 0, 'prob': 0.03}, # Couple + Elderly parent
        {'code': '2A+1S+1C','A': 2, 'S': 1, 'C': 1, 'prob': 0.02}, # 3-Generation home
        {'code': 'Others',  'A': 2, 'S': 0, 'C': 0, 'prob': 0.04}, # Catch-all
    ]
}