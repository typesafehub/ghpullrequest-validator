#!/bin/bash

# run this script after sbt assembly to deploy the new buildbot
ssh ubuntu@buildbot.typesafe.com "cp ~/ghpr2/ghpr-assembly-0.2-SNAPSHOT.jar ~/ghpr2/ghpr-assembly-0.2-SNAPSHOT.jar.old" && \
scp target/scala-2.10/ghpr-assembly-0.2-SNAPSHOT.jar ubuntu@buildbot.typesafe.com:~/ghpr2/ && \
ssh ubuntu@buildbot.typesafe.com sudo initctl restart ghpr2