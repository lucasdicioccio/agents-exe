"""Write the sweep's agent file: an OpenAI-v1 agent whose only tools are a
DeveloperToolbox with read-file-range, write-file-range and patch-file,
sandboxed to the run's work directory.

usage: mkagent.py <work-dir> <out.json> [toolbox-name]

The model comes from MODEL_URL / MODEL_NAME / API_KEY_ID (an id in the
agents-exe secret-keys file, never a key value).
"""
import json, os, sys

work, out = sys.argv[1], sys.argv[2]
box = sys.argv[3] if len(sys.argv) > 3 else "fs"
agent = {"tag": "OpenAIAgentDescription", "contents": {
    "slug": "sweep-files",
    "flavor": "OpenAIv1",
    "modelUrl": os.environ.get("MODEL_URL", "http://mac.home:8000/v1"),
    "apiKeyId": os.environ.get("API_KEY_ID", "workbench-key"),
    "modelName": os.environ.get("MODEL_NAME", "Qwen/Qwen3.6-35B-A3B"),
    "announce": "A file-editing agent.",
    "systemPrompt": ["You are a software agent. You edit files in " + work +
                     " with the developer tools. Do the task, then reply with a one-line summary."],
    "builtinToolboxes": [{"tag": "DeveloperToolbox", "contents": {
        "Name": box,
        "Description": "File tools",
        "Capabilities": ["read-file-range", "write-file-range", "patch-file"],
        "FileSandbox": {"fsbPredicate": {"tag": "DirectoryRecursive", "contents": work},
                        "fsbMaxFileSize": 52428800, "fsbName": "sweep"}}}]}}
json.dump(agent, open(out, "w"), indent=1)
