#!/bin/sh

# Download the information about Proton Pass downloads and their checksums.
wget -O /tmp/version.json https://proton.me/download/PassDesktop/linux/x64/version.json

# Find the newest release.
RELEASE=$(python3 -c """
import json
v = json.load(open('/tmp/version.json'))
r = v['Releases'][0]
f = r['File']
print(f[0]['Url'])
print(f[0]['Sha512CheckSum'])
""")

URL=$(echo $RELEASE | awk '{print $1;}')
CHK=$(echo $RELEASE | awk '{print $2;}')

echo "Downloading release: ${URL}"
wget -O /tmp/proton-pass.deb ${URL}

echo "Checking its checksum.."
echo "${CHK}  /tmp/proton-pass.deb" | shasum -a 512 -c - || exit 1

echo "\nInstalling new release.."
sudo dpkg -i /tmp/proton-pass.deb
