
/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

CREATE TABLE `Comment` (
    `id` BIGINT NOT NULL AUTO_INCREMENT,
    `postId` BIGINT(20) NOT NULL,
    `userId` BIGINT(20) NOT NULL,
    `text` LONGTEXT NOT NULL,
    `creationTime` DATETIME NOT NULL,
    PRIMARY KEY (`id`),
    INDEX `index_User_creationTime` (`creationTime`),
    FOREIGN KEY (postId) REFERENCES Post (id),
    FOREIGN KEY (userId) REFERENCES User (id)
) ENGINE = InnoDB;

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (1, 5, 'Respond yeah economic less sit.', '1996-01-17 00:19:45');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (4, 3, 'Garden team among gun simply upon minute.', '2006-06-27 06:47:17');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (5, 5, 'Over lead series red ready factor almost.', '2001-03-20 18:50:41');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (1, 5, 'Idea nature measure world lot skill friend.', '1980-04-29 19:07:08');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (3, 1, 'Make much fact without.', '2019-06-02 19:33:37');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (1, 1, 'Mind imagine order beat suddenly guess conference.', '2005-04-22 13:49:33');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (1, 2, 'Reason later cup garden election assume page.', '2016-02-21 04:30:46');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (5, 3, 'Suffer role her crime space person decade.', '1973-06-26 22:54:16');

INSERT INTO `Comment` (`postId`, `userId`, `text`, `creationTime`)
    VALUES (5, 3, 'Family choose light sport floor spend room.', '2020-12-18 11:49:52');
