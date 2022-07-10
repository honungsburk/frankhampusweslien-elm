# fhw-app

This is the official app for the frankhampusweslien.com website.
It hosts my art and allows visitors to buy newly minted artworks.

## Dependencies

You need to install

- elm 19.1
- npm
- elm-live

## Download Assets

Download the assets from your own personal Google using the `programming/web-webapp-assets.zip`.
what isn't stored in that folder is stored in your gcp bucket.

## Deployment

To create a preview build:

```bash
make deploy
```

Then you have the option to push to production using

```bash
netlify deploy --prod
```

## Running the App

### Start backend

To run the backend you must have access to nft-maker api.
The key is stored in bitwarden.

```bash
npm run-script nodemon
```

### Start frontend

```bash
npm run-script start
```
