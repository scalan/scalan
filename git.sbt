val ver = "0.2.9-SNAPSHOT"

enablePlugins(GitVersioning)

if( scala.util.Properties.propOrFalse("useGitVersion") ) {
  git.formattedShaVersion := git.gitHeadCommit.value.map({ sha:String => ver + "-" + sha.substring(0,7) })
}
else {
  version in ThisBuild := ver
}
