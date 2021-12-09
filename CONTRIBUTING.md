# Branches, tags and versions

There are two main branches in the `sdp` project repository: `master` and `dev`,
as well as branches for each actual (supported) version. In the course of work,
there may also be additional branches with untested changes and experimental
features.

* `master` - the main branch corresponding to the latest current release (`MAJOR`
or `MINOR`)
* `dev` is the branch corresponding to the last approved change that will go
into the next (`MAJOR` or `MINOR`) release. Commits on the `dev` branch must
comply with the historicity rule: `reset` and similar commands (in particular,
`amend`) aren't allowed, changes are rolled back exclusively changes can only be
rolled back using the `revert` command. Nevertheless, operations with history
can be performed by the project maintainer project - when the project rules are
changed, to clean up and fix history (for example, if a merge error was made).

All `sdp` releases have tags corresponding to their version, for example,
`sdp-0.2` and `sdp-0.2.0.1`. For hackage revisions used `sdp-0.2-revN` tags.

* `MAJOR` releases don't have their own branches: before the release, the
release candidate is on the `dev` branch.
* The story between two `MAJOR` releases may change if some of the changes in
the new `MAJOR` release are ported to the `MINOR` release of the older one.
In any case, such a change shouldn't affect any already published versions and
can only affect the recent history - between the last `MINOR` release of the
previous `MINOR` version and the current `MAJOR` release.
* `MINOR` releases have their own branches on which support work is carried out.
Support for a `MINOR` release continues until a new `MINOR` release of the same
`MAJOR` branch is released (for example, `sdp-0.2.1` may be supported until
`sdp-0.2.2` is released), or until the `MAJOR` version is completely deprecated.
For example, support for the `sdp-0.2.*.*` version (`sdp-0.2` released in
February 2021) will end no earlier than 2025.
* `PATCH` versions have no branches and are published on the `MINOR` release
branches.
* `REVISIONS` can be located both on `MINOR` branch or on their own. Revisions
can be released even for outdated versions if there is an objective need for
them (for example, when changing versions of rarely updated packages).

## Contributing

When contributing to this repository, please first discuss the change you wish
to make via [issue](https://github.com/andreymulik/sdp/issues),
[email](mailto:work.a.mulik@gmail.com?subject=libsdp), or any other method with
the owners of this repository before making a change.

Please write about bugs to [this](mailto:work.a.mulik@gmail.com?subject=libsdp)
mail, because it is the fastest way to deliver information. Practice has shown
that issues can go unnoticed for more than a week.

## Issues

* Please write the version of the library in the issues
* Dependency and/or LTS Haskell versions information might also be helpful
* If the error is platform-dependent, specify the used OS and architecture.
For Windows systems use `winver`, for Unix-like systems `uname -orm` is quite
enough (e.g., `5.15.5-arch1-1 x86_64 GNU/Linux`).

## Merge requests

* Please don't do MR in master.
* If you offer functionality, make MR to `dev` branch or **last** `MINOR` version
branch. MR for irrelevant minor releases are only accepted on special occasions.
* If you suggest fixes for already released versions, please MR to the current
minor branch. If minor branches exist, a new one should be created (e.g.
`sdp-0.3.0` for `sdp-0.3`).

## Languages

* If you find a mistake in the documentation or don't understand something (my
English is very bad), write me an
[email](mailto:work.a.mulik@gmail.com?subject=libsdp_poor_grammar), even if the
problem is in one single letter. Grammar Nazis are welcome here.
* In issue you can write both in English and in Russian (the first is
preferable, because more people will be able to understand you).
* You can write me an e-mail in the language that you know better.
* If you don't know either Russian or English, write me an e-mail in your
language - machine translation is better than translation of a machine
translation.

## About CoС

There is no Code of Conduct, I'm too lazy to write it.


