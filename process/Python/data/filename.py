
from json import dumps as json_dumps
import hashlib
from process.Python import TEST_RUN

def create_hash_filename(params, test_flag: bool = TEST_RUN, filename_suffix: str = None):

    if test_flag:
        return "test"

    # 1. Consistent JSON string
    json_str = json_dumps(params, sort_keys=True).encode('utf-8')
    
    # 2. Generate MD5 hash (unique signature of this dict)
    signature = hashlib.md5(json_str).hexdigest()

    if filename_suffix is not None:
        signature = f"{signature}_{filename_suffix}"

    return signature