# Pull Request Validator
[Fork me, fix me, anyway you improve me!](https://github.com/typesafehub/ghpullrequest-validator)[*](http://www.youtube.com/watch?v=rpRiSb_Ir-s)

Run me: `sbt run-main GhApp $configFile`

## Overview
The Pull Request Validator (PRV) is the middle man between GitHub and Jenkins. It can monitor multiple GitHub repositories and Jenkins instances.
It runs as a separate service, polling both GitHub and Jenkins. (This will change to a push model in the next version.)

Every X minutes, it will scan all repositories to discover new pull requests, determine the merge-branch-to-milestone.

For every PR, it will:

  - set the milestone (if not set yet) according to the merge target of the PR
  - ensure the "reviewed" label is set iff there's a comment that starts with "LGTM" (removing the label when necessary)
  - ensure the "tested" label is set iff all Jenkins jobs have finished successfully for all commits in the PR
  - scan for commands and execute them
  - check all the commits and start Jenkins jobs where necessary



## Flow
The PRV is itself stateless: its state is stored in each commit's status and in Jenkins's build queue.

For every PR:

  1. For every commit that does not have any status, or whose most recent status is FAILURE,
  start a new Job Start Watcher actor, which monitor jenkins for this job to appear (identified by the triple (commit-sha,PR-number,PR-target-branch)),
  starting a new job when necessary.
    1. The actor immediately sets the commit status to PENDING, with an empty target url
    2. The actor polls Jenkins and as soon as the job is building according to Jenkins, a new PENDING status is prepended that has the url to the jenkins job.
  2. For every commit and every PENDING commit status that does not have an ERROR/SUCCESS/PENDINGSUCCESS with the same target url as the pending commit status,
  start a Job Watcher:
    1. When the job aborts, add a FAILURE status to the commit (TODO) and restart the job by posting a PLS REBUILD/$job@$sha comment
    2. When a job ends with an error, prepend an ERROR status (link to the job + mention how long the build took), and comment on the failed commit with a list of the test failures (if we could parse the Jenkins log) and how long the build took.
    3. On a successful end, 
      1. If for all commits, all jobs required to decide the fate of this PR have finished, prepend a SUCCESS status (link to the job + mention how long the build took) to this commit and turn all PENDINGSUCCESS statuses on earlier jobs (if any) into unqualified SUCCESS statuses.
      2. If not all jobs required to decide the fate of this PR have finished, add a PENDINGSUCCESS status.

## Commands
When a command has been found in a PR comment, another comment will be added that tells the PRV to ignore the original comment so that it isn't run twice.

### Rebuilding
There are three variations to trigger the job start watcher and force a new job (or multiple ones) to start:

  - `PLS REBUILD $jobName`
  - `PLS REBUILD/$jobName@$sha`
  - `PLS REBUILD ALL`


### Querying
The following command will add a comment that renders the information stored in the commit statuses for all the commits of this PR:

  - `BUILDLOG?`

### Cleaning up
This command removes all failure comments added to this PR's commits:

  - `NOLITTER!`

## Configuration
TODO: Login info for GitHub and Jenkins. Jenkins jobs to build for every commit.

## Configuration for scala/scala
For [scala/scala](https://github.com/scala/scala), the validator is managed by @adriaanm. It appears on GitHub as scala-jenkins.

The following Jenkins jobs are involved:

  - [pr-rangepos-per-commit](https://scala-webapps.epfl.ch/jenkins/job/pr-rangepos-per-commit/)
  - [pr-checkin-per-commit](https://scala-webapps.epfl.ch/jenkins/job/pr-checkin-per-commit/)

They run the eponymous checkin ant job after doing a detached git checkout of the specified commit, unless the commit has already been merged into one of the master-like branches (i.e., any public branch on scala/scala: master, 2.9.x, 2.10.x, 2.10.1,...). This is to avoid killing Jenkins over a PR that merges e.g., 2.10.x into master. We assume anything merged into these branches has been signed off by the PRV in the first place. Finally, when the given commit corresponds to the PR's branch's HEAD, it will be merged into the PR's target branch so that integration is tested.


## Known Issues
There are some known issues, waiting to be fixed:

  - we compare commit SHAs using String::equals, so that a full sha and an abbreviated one will not be considered equal. GitHub has a similar flaw, where the commit status for the full sha and an abbreviation are not the same.

