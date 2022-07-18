
/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

SELECT Post.title AS postTitle, User.name AS authorName
FROM Post JOIN User ON Post.userId = User.id
ORDER BY Post.creationTime, Post.id
