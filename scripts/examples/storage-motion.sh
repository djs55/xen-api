#!/bin/sh

set -e

VM=$1
DEST_HOST=$2
DEST_SR=$3
DEST_USER=$4
DEST_PASSWORD=$5
if [ -z "$DEST_USER" ]; then
  DEST_USER=root
fi
if [ -z "$DEST_PASSWORD" ]; then
  DEST_PASSWORD=xenroot
fi

SM=/root/sm
if [ ! -x $SM ]; then
  echo I need the binary $SM
  exit 1
fi

URL=http://${DEST_USER}:${DEST_PASSWORD}@${DEST_HOST}/services/SM

echo Snapshotting VM $VM
SNAP=$(xe vm-snapshot uuid=$1 new-name-label=snap-for-migrate)
IFS=","; for vdi in $(xe vbd-list vm-uuid=$SNAP params=vdi-uuid empty=false type=Disk --minimal); do
  echo Copying VDI $vdi to $2
  SR=$(xe vdi-list params=sr-uuid uuid=$vdi --minimal)
  $SM vdi-export $SR $vdi $URL $DEST_SR
done
echo "$(date) VM $VM is going offline now (for suspend)"
xe vm-suspend vm=$1
echo Copying final batch of disk deltaa
IFS=","; for vdi in $(xe vbd-list vm-uuid=$1 params=vdi-uuid empty=false type=Disk --minimal); do
  echo Copying VDI $vdi to $2
  SR=$(xe vdi-list params=sr-uuid uuid=$vdi --minimal)
  $SM vdi-export $SR $vdi $URL $DEST_SR
done
echo Copying over suspend image
vdi=$(xe vm-list uuid=$1 params=suspend-VDI-uuid --minimal)
$SM vdi-export $SR $vdi $URL $DEST_SR
echo Exporting VM metadata
rm -f /tmp/metadata
xe vm-export uuid=$1 filename=/tmp/metadata --metadata include-snapshots=false
echo Importing VM metadata
NEWVM=$(xe -s $DEST_HOST -u $DEST_USER -pw $DEST_PASSWORD vm-import filename=/tmp/metadata --metadata)
echo Resuming VM
xe -s $DEST_HOST -u $DEST_USER -pw $DEST_PASSWORD vm-resume uuid=$NEWVM
echo "$(date) VM $VM is now online"
echo Deleting temporary snapshot
xe snapshot-uninstall snapshot-uuid=$SNAP --force
echo Deleting temporary files
rm -f /tmp/metadata
