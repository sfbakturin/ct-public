
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

SELECT Post.title AS title
FROM Post
WHERE Post.creationTime IN
(
	SELECT MIN(Post.creationTime)
	FROM Post
	WHERE
	(
		SELECT MIN(Post.id)
		FROM Post
	)
	GROUP BY Post.userId
)
GROUP BY Post.userId
ORDER BY Post.creationTime, Post.id
