#!/bin/bash

echo "Starting demo-instance generation of bnk-frontend"
echo "It is $(date)."
echo

# Remove old demo-instace
rm -rf ./repo/dist

# Cloning the repo
echo "Cloning the bnk-frontend repo"
git clone ssh://git@github.com/bnkrs/bnk-frontend.git ./repo 2>&1
echo

# Cloning the apidoc-repo
echo "Cloning the demo-repo, checking out gh-pages"
git clone ssh://git@github.com/bnkrs/demo.git ./repo/dist 2>&1
git checkout gh-pages 2>&1
echo

echo "Changing directory..."
cd ./repo
echo

echo "Moving the .git-directory out of the demo-instance dir"
mv ./dist/.git ./dist-git
echo

echo "Building the demo-instace for production"
npm run build 2>&1 &
PID="$!"
while kill -0 $PID 2> /dev/null; do
    echo "Webpack still running..."
    sleep 60
done
echo

echo "Moving the git-directory back and commiting"
mv ./dist-git ./dist/.git
cd ./dist
git add -A
git commit -m "demo-instance - generated at $(date)" 2>&1

if [ $? -eq 0 ]; then
	echo "commit successful, pushing..."
	git push origin gh-pages 2>&1
	exit 0
else
	echo "commit not successful... exiting"
	exit 1
fi
