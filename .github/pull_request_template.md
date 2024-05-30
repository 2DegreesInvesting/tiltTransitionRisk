

----

TODO

- [ ] Draft the PR title.
- [ ] [Link related issue/PR]([url](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword)). 
- [ ] Draft the PR description.
- [ ] Mark the PR as draft.
- [ ] [Include a unit test](https://code-review.tidyverse.org/reviewer/aspects.html#sec-tests).
- [ ] Review your own PR in "Files changed".
- [ ] Ensure the PR branch is updated.
- [ ] Ensure all checks pass.
- [ ] Change the PR status from draft to ready.
- [ ] Polish the PR description to reflect it's goal (why not how).
- [ ] Polish the PR title as you'd like others to read it from the git log.
- [ ] Assign a reviewer.

Also if the PR includes user-facing changes:

- [ ] Increment the version number: `fledge::update_version()`
- [ ] Update the changelog: `fledge::update_news()`
- [ ] Merge then tag: `fledge::tag_version()`
