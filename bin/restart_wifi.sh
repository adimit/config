#!/bin/sh

echo -n "Switching off wifi, "
nmcli radio wifi off
echo -n "removing drivers, "
sudo rmmod ath10k_pci ath10k_core ath
echo -n "waiting, "
sleep 5s
echo -n "enabling drivers, "
sudo modprobe ath10k_pci
echo -n "waiting, "
sleep 5s
echo "enabling wifi"
nmcli radio wifi on
