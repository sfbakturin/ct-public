
/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

SELECT Post.id AS id
FROM Post
ORDER BY
(
    SELECT COUNT(Comment.postId)
    FROM Comment
    WHERE Comment.postId = Post.id
) DESC, Post.id
