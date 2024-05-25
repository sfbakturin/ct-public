
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

SELECT DISTINCT CAST(Post.creationTime AS date) AS postCreationDate, COUNT(Post.creationTime) AS postCount
FROM Post
GROUP BY CAST(Post.creationTime AS date)
HAVING COUNT(Post.title) >= 1
ORDER BY Post.creationTime
