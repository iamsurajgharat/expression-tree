# expression-tree

Configure as per below setup to publish to Maven Central
https://medium.com/rahasak/publish-scala-library-project-to-maven-central-with-sonatype-d7edaa67d275

On Galago Pro, as the setup is already done, just executed below command in sbt console.
"publishSigned"

Once it is published, go "https://s01.oss.sonatype.org/", verify the depoyed repo, and first close it, once closed, then release it.