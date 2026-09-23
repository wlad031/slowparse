set shell := ["bash", "-euo", "pipefail", "-c"]

# Publishes directly to Gitea Packages and records it without triggering release CI.
publishLocalRegistry version:
  test -n "{{version}}"
  test -z "$(git tag -l "local-v{{version}}")"
  user="${GITEA_USERNAME:-}"; token="${GITEA_TOKEN:-}"; if [[ -z "$user" || -z "$token" ]]; then creds="$(printf 'protocol=https\nhost=gitea.local.vgerasimov.dev\n\n' | git credential fill)"; user="${user:-$(printf '%s\n' "$creds" | sed -n 's/^username=//p')}"; token="${token:-$(printf '%s\n' "$creds" | sed -n 's/^password=//p')}"; fi; test -n "$user"; test -n "$token"; VERSION="{{version}}" GITEA_USERNAME="$user" GITEA_TOKEN="$token" sbt clean test publish
  git tag -a "local-v{{version}}" -m "Local registry publish {{version}}"
  git push origin "local-v{{version}}"
