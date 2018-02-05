package object stackoverflow {
  type Question = Posting
  type Answer = Posting
  type QID = Int
  type HighScore = Int
  type LangIndex = Int

  val questionType = 1
  val answerType = 2
}

/**

<postTypeId>:     Type of the post. Type 1 = question,
                  type 2 = answer.

<id>:             Unique id of the post (regardless of type).

<acceptedAnswer>: Id of the accepted answer post. This
                  information is optional, so maybe be missing
                  indicated by an empty string.

<parentId>:       For an answer: id of the corresponding
                  question. For a question:missing, indicated
                  by an empty string.

<score>:          The StackOverflow score (based on user
                  votes).

<tag>:            The tag indicates the programming language
                  that the post is about, in case it's a
                  question, or missing in case it's an answer.

  */