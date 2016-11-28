#! /bin/ksh
if [ ${#} -ne 2 ]
then
    echo "Err!!"
    echo "Usage: commit_git_repo.sh <file name> <commit comment>"
    exit 1
fi
TS=$(date +%F_%H_%M_%S)
FILE_NM=${1}
COMMIT_MSG=${2}
echo "# Info:${TS}: File added/modified -> ${FILE_NM}" >> README.md
git init
git add ${FILE_NM}
git commit -m "${COMMIT_MSG}"
#git remote add origin git@github.com:git4satya/r_marketing_analytics.git
git push -u origin master
