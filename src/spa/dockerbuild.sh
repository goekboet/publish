#! /bin/bash

echo "Building with image $1"
docker run --mount \
    "type=bind,source=/Users/erikgook/Repos/byappt/apps/publish/src/serverside/wwwroot,destination=/home/publish_spa/pub" \
    $1