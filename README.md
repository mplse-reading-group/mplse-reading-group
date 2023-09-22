## Modifying the Schedule
The schedule for each semester is stored in a markdown file in **schedules/**.
Edit the pipe table, and commit those changes.

The github action should automatically.
## Building the Site
    cabal run -- site rebuild
## Previewing Changes
    cabal run -- site server --hostname HOSTNAME --port PORT
## Deploying
After building the site, make sure to git push **docs/**.

