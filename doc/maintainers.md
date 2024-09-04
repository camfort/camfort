# Notes for maintainers
Things which would otherwise need handing over.

Up to date as of the commit date.

## Versioning and releases (GitHub and Hackage)
Not automated. They used to be partially automated, now no more.

You need the following permissions:

  * GitHub repo write access
  * Hackage package maintainer status
    * Existing maintainers may add other users: click `Package maintainers` on
      the package's Hackage page

Early Git things:

  * be tracking major code changes in `CHANGELOG.md`
  * make the release commit
    * update the package version in `package.yaml`
    * ...then run `hpack` to regenerate the Cabal file
    * update the changelog to tag the new section with its version
    * `git tag` and push the tag

Next, the GitHub release steps. This isn't ultra important, but it's nice.

  * TODO. Depends. If our CI generates binaries, upload them.

Now the Hackage steps:

  * `cabal sdist` and upload the sdist on the Hackage website. (You can also do
    this with Cabal CLI commands, raehik thinks.) The generated sdist is placed
    in `dist-newstyle/sdist/$PKG-$VER.tar.gz`. Upload here:
    https://hackage.haskell.org/upload
  * `cabal haddock --haddock-for-hackage` to build docs. Find in
    `dist-newstyle/`. `gunzip` the generated docs blob, because Hackage takes a
    `.tar`, not `.tar.gz` (silly, raehik knows). On the Hackage package page,
    click `edit package information`, then on the version you just uploaded in
    the `Manage documentation for` list. Upload.

Note that this could be automated. The fiddly Git stuff will differ depending on
your exact repo and standards, so careful not to overengineer (this is why
raehik avoided it).

Note that the Hackage artifacts *may* be trivially generated using CI. However,
raehik's workflow was fiddly, so he disabled it. Consider fixing it if you use a
Stack-based workflow, because raehik believes Stack can't generate Haddock docs.
(Take a look in `.github/workflows/disabled/`.)

### Mac Homebrew release
raehik set up a magic repo in 2021 using a template. All details are here:
https://github.com/camfort/homebrew-camfort

## Nix build (`flake.nix`)
`flake.nix` is a Nix file defining how to build CamFort. raehik uses Nix on his
machine so it's convenient for him. Nix is also really handy for CI, so it's
leveraged in some GitHub Actions workflows. It's also handy for creating Docker
images.

If the Nix build begins to fail and no one on the project is able to assist,
*disable the workflows using Nix and deprecate the Nix build files.* They'll
still be in the history for building old versions, so removing them is OK by
raehik (the Nix build maintainer).

The Nix build GitHub Actions workflow files might have Cachix integration for
magical binary caching. This uses a secret key `CACHIX_AUTH_TOKEN` that needs
configuring per repository. raehik set these up initially. If you need to change
this section, you have the following options:

  1. Remove the Cachix step. You'll just spend more time building the same stuff
     every run.
  2. Make your own Cachix cache and replace the secret key.
  3. Ask raehik.

### Image registry (Docker)
Image registry is a org/person-level thing. raehik had to configure a registry
over [here](https://github.com/camfort/camfort/pkgs/container/camfort), head
over into settings (bottom right), and give `Role: Write` to the
`camfort/camfort` repo to allow CI workflows to push to the registry. No keys
needed.
