## Modifying the Schedule
The schedule for each semester is stored in a markdown file in **schedules/**.
Edit the pipe table, and commit those changes.
(You can do this on the github web interface.)

The github action should automatically build and deploy the site at
<https://mplse-reading-group.github.io>
### Building the Site (manually)
    cabal run -- site rebuild
### Previewing Changes
    cabal run -- site server --hostname HOSTNAME --port PORT
### Deploying
After building the site, make sure to git push **docs/**.

