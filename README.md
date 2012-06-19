# Github Pull Request Validator #
A tool for automatically validating github pull requests with Jenkins


## Configuring ##

Here's an example `ghpr.conf` file.

    scala: {
      jenkins: {
        url = "https://my.company.com/jenkins"
        user: {
          user = "buildbot"
          password = "MY PASSWORD IS SECRET"
        }
        jobs: [ "flush-silly-mistakes" ]
      }
      github: {
        user: {
          user = "gh-commenter"
          password = "MY PASSWORD IS SECRET"
        }
        project: {
          user = "myname"
          project = "myproject"
        }
      }
    }

The outer object is given any random name.  This name represents the "project" name used in logging.  The inside is made up for two config blocks:  `jenkins` and `github`.

The `jenkins` configuration consists of:

* `url` -  The location where we can access Jenkins REST API.  Right now, that's the same as the website URL.
* `user` -  A user/password pair we should use to log in to Jenkins and execute builds.

The `github` configurations consists of:

* `user` - The credentials of a github user that can comments on pull requests.
* `project` - The user/project github combo of which project to monitor.  e.g. "scala/scala" or "jsuereth/scala-arm"


## Setting up Jenkins ##

Any jenkins job will work for validating pull requests, as long as it supports two build parameters:

* `mergebranch` -  The branch to merge the pullrequest *onto*.   This lets us test for merge-related build failures.
* `pullrequest` - The pull request number from github.   This is the code that should be merged in.

Here's an example bash script that, when run inside jenkins on an appropriate repository, can resolve the appropriate git commit:

```
newbranchname="pull-$pullrequest-merge"
# Checkout
git checkout $mergebranch
# Update branch
git pull origin $mergebranch
# Create new merging branch
git checkout -b $newbranchname
# Fetch pull request and merge into new branch.
git fetch origin "+pull/${pullrequest}/head:pullreq/${pullrequest}"
git merge "pullreq/${pullrequest}"
```

## Contributing ##
If you'd like to contribute to the GHPRValidator project, please sign the [contributor's licensing agreement](http://www.typesafe.com/contribute/cla).

## License ##
Copyright 2012 Typesafe, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
