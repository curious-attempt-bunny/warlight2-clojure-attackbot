#!/bin/sh

cat src/bot.clj src/state.clj src/brain.clj src/handlers.clj src/launcher.clj > packaged.clj
zip bot.zip packaged.clj
rm packaged.clj