
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

SELECT DISTINCT userid AS distinctUserIds
FROM Post
WHERE title IS NOT NULL
ORDER BY userid ASC
