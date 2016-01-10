# hs-carbon-aggregator [![Build Status](https://travis-ci.org/ratsam/hs-carbon-aggregator.svg?branch=master)](https://travis-ci.org/ratsam/hs-carbon-aggregator)

#### Like `carbon-aggregator.py` only multi-threaded.
Designed as drop-in replacement for `carbon-aggregator.py` with fine-tuned native concurrency.

<img src="https://raw.githubusercontent.com/ratsam/hs-carbon-aggregator/master/readme-resources/multicore.jpg" width="250"/>  
`carbon-aggregator.py` at work

#### Differences and limitations

- Original Carbon Aggregator daemon reloads `aggregation-rules.conf` if file changes, while HS Carbon Aggregator does not.

- Please note that HS Carbon Aggregator only supports Line and Pickle interfaces. AMQP interface isn't supported yet.

- Instances management isn't supported yet.
This means that `hs-carbon-aggregator` won't recognize `[aggregator:x]` configuration sections.  
Feature to be supported soon.

- Rewrite rules aren't supported and unlikely to be supported soon.

#### Some important configuration options that aren't supported yet
`REPLICATION_FACTOR`  
`MAX_QUEUE_SIZE`  
`USE_FLOW_CONTROL`  
`WRITE_BACK_FREQUENCY`  
`USE_WHITELIST`  
`LOG_LISTENER_CONN_SUCCESS`  
`LOG_AGGREGATOR_MISSES`  
`RELAY_RULES`  

Please let me know if your setup requires any specific configuration options and I'll add them.

## Building from sources
You will need Haskell and Cabal installed, e.g. from [Haskell Platform](https://www.haskell.org/platform/).  
Then:
```bash
git clone https://github.com/ratsam/hs-carbon-aggregator.git
cd hs-carbon-aggregator
cabal sandbox init
cabal install --only-dependencies --enable-tests --reorder-goals -j
cabal build
```
Default path for executable is `dist/build/carbon-aggregator/`.
