---
layout: post
title: "Fetching Assigned Reviews from GitHub v4 (graphql) API"
date: 2019-07-04 19:16:00 +0000
author: "Chris Schneider"
comments: true
categories:
  - Hacks
---

Working on optimizing my workflow with the rest of the team, I wanted to build
a little tool to fetch all the PRs that somebody has explicitly asked for my
review on. I did a little research, and played with the GitHub API, but didn't
find anything. I sent a quick message to their support, which was responded to
almost immediately. The trick was to use the generic 'Search' query, instead of looking for a specifically built graphql sequence.

Hopefully this will help anybody who's looking for something similar.

```
{
  search(type:ISSUE, query:"is:open review-requested:cschneid", first:100) {
    edges {
      node {
        ... on PullRequest {
          title
          url
        }
      }
    }
  }
}
```

More details to fine tune your [search query](https://help.github.com/en/articles/searching-issues-and-pull-requests#search-by-pull-request-review-status-and-reviewer).
