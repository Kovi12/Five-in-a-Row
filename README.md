# Five-in-a-Row in Scala

Console Five-in-a-Row with an AI opponent using minimax and alpha-beta pruning.

The board is represented with immutable lists and player case objects. The search includes a heuristic evaluation and move ordering; sound feedback is supported through Java's audio API.

## Run

Use a JDK and sbt, then run `sbt run`. The Scala version is defined in `build.sbt`. Enter moves as row/column digits. The interactive loop currently assumes the human plays X. Supply `/sound/magic.wav` on the classpath to use the sound feature.

University functional-programming project. Course foundations and supplied exercises retain their original attribution.
