# codacy-metrics-lizard

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/9e4c1f4395ac4ffa962793398aea39fe)](https://www.codacy.com/app/Codacy/codacy-metrics-lizard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=codacy/codacy-metrics-lizard&amp;utm_campaign=Badge_Grade)
[![CircleCI](https://circleci.com/gh/codacy/codacy-metrics-lizard.svg?style=svg)](https://circleci.com/gh/codacy/codacy-metrics-lizard)

This is the docker engine we use at Codacy to have [Lizard](https://github.com/terryyin/lizard) support.

## Usage

You can create the docker by doing:

```bash
sbt docker:publishLocal
```

The docker is ran with the following command:

```bash
docker run -it -v $srcDir:/src  <DOCKER_NAME>:<DOCKER_VERSION>
docker run -it -v $PWD/src/test/resources:/src codacy/codacy-metrics-cloc:latest
```

## Test

Before running the tests, you need to install Lizard. The tool [README](https://github.com/terryyin/lizard/blob/master/README.rst) provides you instructions on how to do so.

After that, you can run the tests:

```bash
sbt test
```

## What is Codacy

[Codacy](https://www.codacy.com/) is an Automated Code Review Tool that monitors your technical debt, helps you improve your code quality, teaches best practices to your developers, and helps you save time in Code Reviews.

### Among Codacy’s features

- Identify new Static Analysis issues
- Commit and Pull Request Analysis with GitHub, BitBucket/Stash, GitLab (and also direct git repositories)
- Auto-comments on Commits and Pull Requests
- Integrations with Slack, HipChat, Jira, YouTrack
- Track issues in Code Style, Security, Error Proneness, Performance, Unused Code and other categories

Codacy also helps keep track of Code Coverage, Code Duplication, and Code Complexity.

Codacy supports PHP, Python, Ruby, Java, JavaScript, and Scala, among others.

### Free for Open Source

Codacy is free for Open Source projects.