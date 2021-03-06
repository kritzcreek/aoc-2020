module Input where

import Prelude

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int

instance showInstruction :: Show Instruction where
  show = case _ of
    Nop _ -> "nop"
    Acc x -> "acc " <> show x
    Jmp x -> "jmp " <> show x

input :: Array Instruction
input =
  [Jmp (236),
  Acc (43),
  Acc (28),
  Jmp (149),
  Acc (28),
  Acc (13),
  Acc (36),
  Acc (42),
  Jmp (439),
  Acc (-14),
  Jmp (29),
  Jmp (154),
  Acc (16),
  Acc (-13),
  Acc (-16),
  Nop (317),
  Jmp (497),
  Acc (21),
  Jmp (386),
  Jmp (373),
  Acc (22),
  Jmp (311),
  Acc (-16),
  Acc (27),
  Acc (21),
  Acc (43),
  Jmp (512),
  Jmp (218),
  Jmp (217),
  Acc (12),
  Acc (44),
  Nop (367),
  Nop (180),
  Jmp (134),
  Acc (-2),
  Acc (42),
  Acc (13),
  Acc (-11),
  Jmp (442),
  Nop (457),
  Jmp (151),
  Acc (15),
  Acc (-4),
  Acc (0),
  Jmp (131),
  Acc (6),
  Acc (-2),
  Acc (37),
  Jmp (112),
  Acc (32),
  Acc (6),
  Acc (-15),
  Jmp (474),
  Jmp (515),
  Acc (12),
  Acc (11),
  Acc (4),
  Jmp (339),
  Acc (-3),
  Acc (36),
  Jmp (220),
  Nop (91),
  Acc (-12),
  Jmp (49),
  Acc (-17),
  Jmp (204),
  Acc (40),
  Jmp (535),
  Acc (37),
  Acc (8),
  Nop (147),
  Nop (174),
  Jmp (306),
  Jmp (305),
  Acc (7),
  Acc (33),
  Jmp (305),
  Acc (22),
  Acc (17),
  Acc (24),
  Jmp (458),
  Jmp (1),
  Acc (36),
  Acc (34),
  Jmp (113),
  Acc (-3),
  Nop (113),
  Nop (-34),
  Jmp (506),
  Acc (-19),
  Acc (21),
  Acc (35),
  Acc (-1),
  Jmp (74),
  Acc (15),
  Acc (7),
  Jmp (79),
  Acc (29),
  Acc (42),
  Jmp (427),
  Acc (33),
  Jmp (29),
  Acc (6),
  Acc (13),
  Nop (477),
  Acc (26),
  Jmp (493),
  Acc (33),
  Acc (43),
  Acc (49),
  Acc (35),
  Jmp (409),
  Acc (-7),
  Acc (35),
  Acc (40),
  Jmp (309),
  Acc (-13),
  Acc (-14),
  Acc (32),
  Jmp (322),
  Jmp (10),
  Jmp (44),
  Acc (20),
  Acc (25),
  Jmp (175),
  Acc (22),
  Acc (16),
  Acc (1),
  Acc (36),
  Jmp (-65),
  Jmp (231),
  Acc (35),
  Jmp (155),
  Jmp (218),
  Acc (-10),
  Acc (-13),
  Acc (38),
  Jmp (-92),
  Acc (15),
  Jmp (134),
  Acc (-16),
  Acc (18),
  Jmp (-30),
  Nop (-41),
  Acc (48),
  Acc (49),
  Jmp (-107),
  Acc (4),
  Acc (34),
  Acc (38),
  Acc (-18),
  Jmp (247),
  Acc (45),
  Acc (23),
  Jmp (149),
  Nop (164),
  Acc (26),
  Jmp (-24),
  Jmp (240),
  Jmp (77),
  Acc (30),
  Acc (-13),
  Jmp (-158),
  Nop (136),
  Jmp (33),
  Jmp (189),
  Jmp (143),
  Jmp (1),
  Acc (4),
  Acc (30),
  Jmp (-106),
  Acc (16),
  Nop (-52),
  Acc (37),
  Jmp (119),
  Acc (-11),
  Acc (-9),
  Acc (15),
  Acc (4),
  Jmp (301),
  Jmp (1),
  Acc (-3),
  Jmp (188),
  Nop (86),
  Nop (125),
  Acc (-10),
  Jmp (-105),
  Acc (36),
  Acc (9),
  Acc (0),
  Jmp (317),
  Jmp (347),
  Acc (48),
  Nop (380),
  Acc (-18),
  Acc (28),
  Jmp (398),
  Jmp (-152),
  Jmp (-86),
  Acc (22),
  Acc (11),
  Acc (39),
  Jmp (-173),
  Jmp (343),
  Nop (194),
  Nop (98),
  Nop (382),
  Jmp (300),
  Acc (35),
  Nop (287),
  Acc (-8),
  Jmp (302),
  Acc (19),
  Acc (45),
  Jmp (95),
  Acc (29),
  Jmp (274),
  Acc (18),
  Acc (-13),
  Acc (23),
  Acc (7),
  Jmp (164),
  Acc (17),
  Acc (36),
  Acc (-5),
  Jmp (153),
  Acc (21),
  Jmp (105),
  Jmp (1),
  Nop (267),
  Jmp (277),
  Jmp (88),
  Acc (2),
  Acc (18),
  Nop (182),
  Jmp (189),
  Acc (37),
  Acc (46),
  Jmp (258),
  Acc (22),
  Acc (15),
  Jmp (249),
  Acc (17),
  Jmp (-162),
  Jmp (25),
  Acc (-6),
  Nop (314),
  Jmp (-30),
  Jmp (312),
  Acc (34),
  Nop (-230),
  Acc (-2),
  Jmp (158),
  Acc (-4),
  Acc (37),
  Jmp (318),
  Acc (18),
  Acc (23),
  Acc (-8),
  Jmp (-248),
  Jmp (181),
  Acc (17),
  Acc (4),
  Jmp (-189),
  Acc (27),
  Acc (-13),
  Acc (-4),
  Acc (8),
  Jmp (222),
  Jmp (310),
  Acc (-5),
  Acc (35),
  Jmp (241),
  Jmp (-130),
  Jmp (124),
  Acc (-19),
  Jmp (331),
  Acc (-8),
  Acc (45),
  Jmp (106),
  Acc (23),
  Acc (48),
  Jmp (-107),
  Acc (7),
  Acc (-19),
  Acc (3),
  Jmp (130),
  Jmp (-104),
  Nop (5),
  Acc (29),
  Acc (8),
  Acc (-6),
  Jmp (7),
  Acc (12),
  Jmp (102),
  Acc (-4),
  Acc (46),
  Acc (-17),
  Jmp (-209),
  Acc (20),
  Jmp (-271),
  Acc (48),
  Jmp (30),
  Nop (204),
  Acc (-19),
  Acc (4),
  Acc (38),
  Jmp (17),
  Jmp (116),
  Acc (-17),
  Acc (23),
  Jmp (-75),
  Jmp (-129),
  Jmp (152),
  Acc (36),
  Nop (-193),
  Acc (26),
  Acc (38),
  Jmp (242),
  Jmp (-197),
  Acc (32),
  Acc (-5),
  Acc (-19),
  Jmp (-201),
  Jmp (-304),
  Acc (9),
  Jmp (175),
  Acc (1),
  Jmp (-15),
  Jmp (1),
  Nop (-74),
  Jmp (-38),
  Nop (-165),
  Acc (-19),
  Jmp (-317),
  Acc (-19),
  Acc (-1),
  Jmp (17),
  Acc (0),
  Nop (151),
  Jmp (93),
  Acc (32),
  Acc (29),
  Acc (0),
  Jmp (-340),
  Acc (39),
  Jmp (-115),
  Acc (0),
  Acc (47),
  Nop (-320),
  Jmp (244),
  Acc (29),
  Jmp (81),
  Jmp (-84),
  Acc (2),
  Acc (16),
  Nop (-345),
  Acc (23),
  Jmp (9),
  Acc (26),
  Jmp (-67),
  Acc (-11),
  Acc (38),
  Jmp (150),
  Acc (19),
  Acc (-2),
  Jmp (-244),
  Jmp (88),
  Acc (-4),
  Jmp (-157),
  Acc (22),
  Acc (33),
  Acc (41),
  Jmp (-117),
  Acc (31),
  Acc (50),
  Acc (24),
  Jmp (-265),
  Jmp (1),
  Jmp (-352),
  Jmp (-312),
  Acc (35),
  Acc (30),
  Jmp (-90),
  Jmp (8),
  Acc (14),
  Acc (39),
  Jmp (-112),
  Acc (-11),
  Acc (-3),
  Acc (22),
  Jmp (-116),
  Acc (48),
  Jmp (-194),
  Acc (-5),
  Jmp (-252),
  Jmp (66),
  Jmp (-295),
  Jmp (196),
  Acc (25),
  Acc (-11),
  Nop (112),
  Acc (33),
  Jmp (123),
  Acc (-10),
  Acc (28),
  Nop (-119),
  Acc (12),
  Jmp (-166),
  Jmp (-356),
  Acc (8),
  Acc (16),
  Jmp (161),
  Acc (25),
  Acc (3),
  Jmp (-5),
  Acc (32),
  Acc (40),
  Jmp (181),
  Acc (-11),
  Acc (-5),
  Jmp (1),
  Acc (0),
  Jmp (-265),
  Acc (5),
  Acc (24),
  Acc (15),
  Acc (-17),
  Jmp (-326),
  Nop (103),
  Acc (-9),
  Acc (13),
  Jmp (-379),
  Acc (38),
  Acc (16),
  Jmp (-65),
  Jmp (1),
  Jmp (1),
  Jmp (1),
  Acc (-1),
  Jmp (-191),
  Acc (35),
  Acc (-19),
  Acc (-6),
  Jmp (-52),
  Acc (15),
  Jmp (-357),
  Nop (-134),
  Acc (-3),
  Nop (103),
  Jmp (-123),
  Acc (43),
  Acc (0),
  Acc (47),
  Jmp (-373),
  Acc (0),
  Acc (50),
  Acc (44),
  Acc (21),
  Jmp (-114),
  Acc (-19),
  Jmp (-339),
  Acc (25),
  Jmp (-410),
  Jmp (-126),
  Acc (-2),
  Acc (-6),
  Acc (14),
  Jmp (-207),
  Acc (35),
  Acc (-7),
  Jmp (75),
  Acc (9),
  Acc (22),
  Jmp (114),
  Acc (18),
  Acc (36),
  Acc (0),
  Acc (40),
  Jmp (-192),
  Acc (35),
  Acc (0),
  Acc (28),
  Acc (3),
  Jmp (-346),
  Nop (-131),
  Acc (46),
  Nop (-467),
  Nop (-179),
  Jmp (-151),
  Jmp (-120),
  Acc (30),
  Acc (22),
  Acc (-7),
  Acc (18),
  Jmp (-157),
  Acc (5),
  Jmp (76),
  Nop (-315),
  Acc (25),
  Jmp (-357),
  Acc (44),
  Jmp (-12),
  Acc (0),
  Acc (19),
  Nop (-485),
  Jmp (-495),
  Nop (-115),
  Acc (12),
  Jmp (-8),
  Acc (31),
  Acc (-7),
  Jmp (-158),
  Acc (44),
  Acc (32),
  Jmp (87),
  Acc (1),
  Acc (37),
  Acc (44),
  Jmp (-86),
  Acc (0),
  Acc (17),
  Acc (-13),
  Jmp (-434),
  Acc (37),
  Jmp (-342),
  Acc (3),
  Jmp (1),
  Acc (29),
  Jmp (-242),
  Acc (48),
  Jmp (-442),
  Jmp (-283),
  Acc (-19),
  Acc (6),
  Acc (20),
  Acc (44),
  Jmp (-533),
  Acc (-15),
  Nop (-356),
  Acc (18),
  Jmp (-408),
  Acc (-9),
  Acc (17),
  Acc (16),
  Jmp (-385),
  Nop (-130),
  Jmp (1),
  Acc (38),
  Acc (39),
  Jmp (-324),
  Jmp (-141),
  Acc (4),
  Acc (3),
  Acc (-4),
  Jmp (-114),
  Acc (2),
  Jmp (1),
  Acc (44),
  Jmp (-360),
  Acc (43),
  Acc (36),
  Nop (-177),
  Nop (-288),
  Jmp (-496),
  Acc (45),
  Acc (0),
  Jmp (-322),
  Acc (13),
  Jmp (-511),
  Acc (-2),
  Acc (36),
  Jmp (-460),
  Acc (28),
  Acc (28),
  Jmp (-455),
  Acc (-4),
  Acc (38),
  Jmp (-145),
  Jmp (-163),
  Jmp (-331),
  Nop (-227),
  Jmp (-470),
  Acc (35),
  Nop (-419),
  Acc (39),
  Acc (0),
  Jmp (-435),
  Jmp (1),
  Jmp (-69),
  Acc (20),
  Acc (46),
  Nop (2),
  Jmp (-239),
  Acc (-3),
  Acc (12),
  Acc (38),
  Jmp (-259),
  Jmp (-60),
  Jmp (-67),
  Nop (-542),
  Jmp (-397),
  Acc (32),
  Jmp (-57),
  Acc (30),
  Nop (-393),
  Jmp (-380),
  Acc (16),
  Acc (-7),
  Acc (0),
  Acc (2),
  Jmp (1)]
