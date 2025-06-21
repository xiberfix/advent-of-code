#!/usr/bin/env python
import os
import sys
import time
import urllib.request
from datetime import datetime

year = int(sys.argv[1]) if len(sys.argv) > 1 else datetime.now().year
print(f"{year}")

try:
    with open("session.txt") as f:
        session = f.read().strip()
except Exception as e:
    print(f"❌ failed to read session token: {e}")
    sys.exit(1)

if len(session) == 0:
    print("❌ session token is missing")
    sys.exit(1)

os.makedirs(f"{year}", exist_ok=True)

for day in range(1, 26):
    try:
        filename = f"{year}/{day:02}.txt"
        if os.path.exists(filename):
            print(f"⚪ {day:02} already exists")
            continue

        url = f"https://adventofcode.com/{year}/day/{day}/input"
        req = urllib.request.Request(url, headers={"Cookie": f"session={session}"})
        with urllib.request.urlopen(req) as resp:
            data = resp.read().decode()
        with open(filename, "w") as f:
            f.write(data)
        time.sleep(1)

        print(f"🟢 {day:02} downloaded")
    except Exception as e:
        print(f"🔴 {day:02} failed: {e}")
