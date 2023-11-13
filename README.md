## Modifying the Schedule
The schedule for each semester is stored in a yaml file in **schedules/**.

Edit the yaml "table", and commit those changes.
The table is an array of arrays, where each element of the top-level array is an array of:
```
- speaker name
- time ∧ location
- paper title
- link to paper
- abstract
```
Take a look at an existing schedule for examples.

You can do this on the github web interface.

The github workflow should automatically build and deploy the site at
<https://mplse-reading-group.github.io>
### Manual Intervention
#### Building the Site Manually
    cabal update
    cabal build
    cabal run -- site rebuild
#### Previewing Changes
    cabal run -- site watch --hostname HOSTNAME --port PORT
#### Deploying
The github workflow builds the site from *trunk* and copies the generated site contents from **docs/** to the branch *gh-pages*.
Github pages then serves *gh-pages*.

You could manually build the site and push to *gh-pages*.
(But the next push to *trunk* will probably overwrite whatever you did.)

