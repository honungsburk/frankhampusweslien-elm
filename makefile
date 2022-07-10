#\!/bin/sh

serve:
	elm-live src/Main.elm --open --pushstate --hot -d public --proxyhost=http://localhost:3000 --proxyPrefix=/.netlify -- --output=public/elm.js 

deploy:
	@elm-test
	@elm make src/Main.elm --optimize --output=public/elm.js 
	@npm run-script build
	netlify deploy