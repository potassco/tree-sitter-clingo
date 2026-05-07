#!/bin/bash

set -e

read -rp "Enter the new version: " version

if ! [[ "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
	echo "Error: version must be in the format x.y.z"
	exit 1
fi

sed -i "s/^version = \".*\"/version = \"$version\"/" Cargo.toml pyproject.toml
sed -i "s/^VERSION := [^ ]*/VERSION := $version/" Makefile
sed -i "s/\"version\": \".*\"/\"version\": \"$version\"/" package.json

set -ex

npm update -S
cargo update
