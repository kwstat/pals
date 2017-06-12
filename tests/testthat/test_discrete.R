
require(pals)

context("test_discrete.R")

# alphabet
expect_message(pal.bands(alphabet(27))) # too many

pal.bands(alphabet(1),alphabet(2),alphabet(3),alphabet(4),alphabet(5),alphabet(6),
          alphabet(7),alphabet(8),alphabet(9),alphabet(10),alphabet(11),alphabet(12),
          alphabet(13),alphabet(14),alphabet(15),alphabet(16),alphabet(17),alphabet(18),
          alphabet(19),alphabet(20),alphabet(21),alphabet(22),alphabet(23),alphabet(24),
          alphabet(25),alphabet(26),
          labels=1:26, main="alphabet")

# alphabet2
expect_message(pal.bands(alphabet2(27))) # too many

pal.bands(alphabet2(1),alphabet2(2),alphabet2(3),alphabet2(4),alphabet2(5),alphabet2(6),
          alphabet2(7),alphabet2(8),alphabet2(9),alphabet2(10),alphabet2(11),alphabet2(12),
          alphabet2(13),alphabet2(14),alphabet2(15),alphabet2(16),alphabet2(17),alphabet2(18),
          alphabet2(19),alphabet2(20),alphabet2(21),alphabet2(22),alphabet2(23),alphabet2(24),
          alphabet2(25),alphabet2(26),
          labels=1:26, main="alphabet2")


# cols25
expect_message(pal.bands(cols25(26))) # too many

pal.bands(cols25(1),cols25(2),cols25(3),cols25(4),cols25(5),cols25(6),
          cols25(7),cols25(8),cols25(9),cols25(10),cols25(11),cols25(12),
          cols25(13),cols25(14),cols25(15),cols25(16),cols25(17),cols25(18),
          cols25(19),cols25(20),cols25(21),cols25(22),cols25(23),cols25(24),
          cols25(25),
          labels=1:25, main="cols25")


# glasbey
expect_message(pal.bands(glasbey(33))) # too many

pal.bands(glasbey(1),glasbey(2),glasbey(3),glasbey(4),glasbey(5),glasbey(6),
          glasbey(7),glasbey(8),glasbey(9),glasbey(10),glasbey(11),glasbey(12),
          glasbey(13),glasbey(14),glasbey(15),glasbey(16),glasbey(17),glasbey(18),
          glasbey(19),glasbey(20),glasbey(21),glasbey(22),glasbey(23),glasbey(24),
          glasbey(25),glasbey(26),glasbey(27),glasbey(28),glasbey(29),glasbey(30),
          glasbey(31),glasbey(32),
          labels=1:32, main="glasbey")


# kelly
expect_message(pal.bands(kelly(23))) # too many

pal.bands(kelly(1),kelly(2),kelly(3),kelly(4),kelly(5),kelly(6),
          kelly(7),kelly(8),kelly(9),kelly(10),kelly(11),kelly(12),
          kelly(13),kelly(14),kelly(15),kelly(16),kelly(17),kelly(18),
          kelly(19),kelly(20),kelly(21),kelly(22),
          labels=1:22, main="kelly")

# polychrome
expect_message(pal.bands(polychrome(37))) # too many

pal.bands(polychrome(1),polychrome(2),polychrome(3),polychrome(4),
          polychrome(5),polychrome(6),polychrome(7),polychrome(8),
          polychrome(9),polychrome(10),polychrome(11),polychrome(12),
          polychrome(13),polychrome(14),polychrome(15),polychrome(16),
          polychrome(17),polychrome(18),polychrome(19),polychrome(20),
          polychrome(21),polychrome(22),polychrome(23),polychrome(24),
          polychrome(25),polychrome(26),polychrome(27),polychrome(28),
          polychrome(29),polychrome(30),polychrome(31),polychrome(32),
          polychrome(33),polychrome(34),polychrome(35),polychrome(36),
          labels=1:36, main="polychrome")

# stepped
expect_message(pal.bands(stepped(25))) # too many

pal.bands(stepped(1),stepped(2),stepped(3),stepped(4),stepped(5),stepped(6),
          stepped(7),stepped(8),stepped(9),stepped(10),stepped(11),stepped(12),
          stepped(13),stepped(14),stepped(15),stepped(16),stepped(17),stepped(18),
          stepped(19),stepped(20),stepped(21),stepped(22),stepped(23),stepped(24),
          labels=1:24, main="stepped")

# tol
expect_message(pal.bands(tol(13))) # too many

pal.bands(tol(1),tol(2),tol(3),tol(4),tol(5),tol(6),
          tol(7),tol(8),tol(9),tol(10),tol(11),tol(12),
          labels=1:12, main="tol")

# watlington
expect_message(pal.bands(watlington(17))) # too many

pal.bands(watlington(1),watlington(2),watlington(3),watlington(4),watlington(5),watlington(6),
          watlington(7),watlington(8),watlington(9),watlington(10),watlington(11),watlington(12),
          watlington(13),watlington(14),watlington(15),watlington(16),
          labels=1:16, main="watlington")

# bivariate
expect_message(pal.bands(kelly(10)))
expect_message(stevens.pinkgreen(10))
expect_message(stevens.bluered(10))
expect_message(stevens.pinkblue(10))
expect_message(stevens.greenblue(10))
expect_message(stevens.purplegold(10))
expect_message(brewer.orangeblue(10))
expect_message(brewer.pinkblue(10))
expect_message(tolochko.redblue(10))
expect_message(arc.bluepink(17))
expect_message(census.blueyellow(10))
