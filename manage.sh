#!/bin/sh
dirs=$(ls -d haskus-*/ | cut -d'/' -f1)

function last_tag {
   git tag -l | grep "$1-[0-9]" | sort -r -V | head -n 1
}

function check_tag {
   local t=$(git tag -l | grep "$1")
   if [ "$t" = "" ]
      then echo "NO"
      else echo "YES"
   fi
}

function is_uploaded {
   local v=$(curl -s -I https://hackage.haskell.org/package/$1 | head -n 1 | cut -d' ' -f2)
   if [ "$v" = "200" ]
      then echo "YES"
      else if [ "$v" = "404" ]
         then echo "NO"
         else echo "DON'T KNOW"
      fi
   fi
}

function package_version {
   ver=$(cd $1 && stack query locals $1 version)
   echo $ver | cut -d"'" -f2
}

function report {
   tag=$(last_tag $1)
   echo ""
   echo "---------------------------------------------------------------"
   echo "$1:"
   echo "  - Last tag: $tag"
   echo "  - Last tag hackage uploaded: $(is_uploaded $tag)"
   echo "  - Dev version: $(package_version $1)"
   echo "  - Log since last tag:"
   git --no-pager log --oneline $tag..HEAD -- $1/
   echo "---------------------------------------------------------------"
}

function report_all {
   echo "==============================================================="
   echo "Reporting package infos"
   echo "==============================================================="
   for i in $dirs
   do
      report $i
   done
}

function check_resolvers {
   echo "==============================================================="
   echo "Checking resolvers"
   echo "==============================================================="
   yamls=$(ls haskus-**/stack.yaml)
   resolver=""

   for yaml in $yamls
   do
      r=$(cat $yaml | grep resolver | cut -d':' -f2 | xargs)
      echo "Found resolver: $r ($yaml)" 
      if [ "$resolver" = "" ]
         then resolver=$r
         else if [ "$resolver" != "$r" ]
               then echo "Different resolvers found: $resolver $r" && exit 1
              fi
      fi
   done

   echo "OK: using resolver $resolver"
}

function build {
   echo "==============================================================="
   echo "Building $1"
   echo "==============================================================="

   result=$(cd $1 && stack clean && stack test)
   if [ $? -ne 0 ]
      then echo "Error! Fix it and press a key. " && read -n 1 -s && build $1
   fi
   echo $result
}

function build_all {
   for i in $dirs
   do
      build $i
   done
}

function showdone {
   echo ""
   echo "==============================================================="
   echo "Done"
   echo "==============================================================="
}

function check_dev_version {
   echo "Package: $1"
   local v="$(package_version $1)"
   echo "  - Version: $v"
   local nt="$1-$v"
   local upl=$(is_uploaded $nt)
   echo "  - Already on Hackage: $upl"
   local ct=$(check_tag $nt)
   echo "  - Tag exist: $ct"
   if [ "$ct" = "NO" ]
      then echo "You need to create the tag $nt" && exit 1
   fi
}

function check_dev_versions {
   echo "==============================================================="
   echo "Checking release versions"
   echo "==============================================================="
   for i in $dirs
   do
      check_dev_version $i
   done
}

function check_rep_state {
   echo "==============================================================="
   echo "Checking repository state"
   echo "==============================================================="

   local r=$(git status -s --untracked-files=no)
   if [ "$r" != "" ]
      then echo "Repository isn't clean:" && git status && exit 1
      else echo "Repositry is clean"
   fi
}

case "$1" in
   report)
      report_all
      showdone
      ;;
   check)
      check_resolvers
      check_dev_versions
      showdone
      ;;
   build)
      build_all
      showdone
      ;;
   release)
      check_resolvers
      check_dev_versions
      check_rep_state
      report_all
      build_all
      showdone
      ;;
   *)
      echo "Missing command: check, report, build, release"
      exit 0
esac
