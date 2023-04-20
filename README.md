# Mage Log Analyzer 

Analyze your logs for performance and gameplay


## Live site

https://wotlk-mage.herokuapp.com/


## Current features

- Spec detection (FFB/TTW) - Other specs (Frost and Arcane) are being worked.

- Measure the amount of munch/vomit in your ignite.
    - Additionally, delay metrics for TTW mages to reduce munching. 

- Cast metrics:
    - Clipped living bombs.
    - Interrupted pyros/fireballs/FFBs.
    - Hotstreak data.

- Resource summary:
    - HP/Mana and hit/spellpower across the encounter.
    
- Notifications and alerts:
    - The app will color/notify you if any of the following values are abnormal
          - Severely under or over hitcap.
          - Munching WA not working as TTW.
          - Missing relevant enchants.
          - Living bombs clipped.
          - Sub-optimal use of hotstreaks.
          - Excesive damage lost from ignite not ticking due to target death.


## Run locally

You can in theory run the app locally with the files in the "local" folder. However, you will need to set-up a proper token and API v2 key on lumberjack website.

You also need to run R and install a couple packages. I think?





