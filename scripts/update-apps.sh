#!/bin/sh
echo "Checking for package updates via APT, Snap, and Flatpak."

echo "\n======= APT ======="
sudo apt update
sudo apt list --upgradable
sudo apt upgrade

echo "\n======= Snap ======="
sudo snap refresh

echo "\n======= Flatpak ======="
flatpak update
