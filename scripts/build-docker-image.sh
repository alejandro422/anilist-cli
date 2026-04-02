#!/usr/bin/env sh

set -eu

image_name="${IMAGE_NAME:-fuwn/anilist}"
platforms="${PLATFORMS:-linux/amd64,linux/arm64}"
push_flag="${PUSH_FLAG:---push}"
utc_date_tag="$(date -u +%F)"

docker buildx build \
	--platform "${platforms}" \
	--tag "${image_name}:latest" \
	--tag "${image_name}:${utc_date_tag}" \
	"${push_flag}" \
	.
