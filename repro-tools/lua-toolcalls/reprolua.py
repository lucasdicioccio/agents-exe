import json
import sys

dat = ""
with open(sys.argv[1], "r") as f:
  dat = f.read()

print(json.dumps({"script":dat}))
