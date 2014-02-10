\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #96
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key c \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 \slurDashed\tieDashed
  g'8~ g |
  c4 c b8~ b b8~ b |
  a4 a g b\rest |
  e,8 d c d e4 a |
  g2 b4\rest g |
  c c8~ c b4 b8~ b |
  
  a4 a g e8[ f] |
  g4 e8( d) c4 a'8~ a |
  << {g4. s8} {s4 \teeny e8~ \normalsize e} >> c4 b'\rest |
  b8 b a g d'4 d |
  
  g,2 << {b\rest} {s4 \teeny g8~ g} >> | \normalsize
  g8( a) a~ a a4 b8( a) |
  g8~ g8~ g8~ g8 g4 g8~ g |
  a4 a8~ a a4 b8[ a] |
  \tieSolid
  g4 g g a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 b'\rest |
  b8 b b b b4 fis |
  \slurSolid
  b2( cis) |
  d4-- e-- d-- b-- |
  
  \acciaccatura {a16[ b]} a2. g4 |
  \time 3/4
  g2.~^\markup\italic"rall." |
  g4 b4. a8 |
  g2^\markup\italic"a tempo" g4 |
  g8 b\rest b4 a |
  g2 g4 |
  g4 b2\rest |
  g4 e g |
  c( g) c |
  
  d2.~^\markup\italic"rall." |
  d4 e4.\fermata d8 |
  c2 g4 |
  e8 b'\rest b4 a |
  g2 f4 |
  d8 b'\rest e,4 g |
  c g c |
  d2 c4 |
  c2.~ |
  c4 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	’Twas in fif -- ty -- five, _ on a win -- ter’s night,
  Cheer -- i -- ly my lads yo -- ho!
  We’d got the __ _ Roo -- shan _ lines in sight,
  When _ up comes a lit -- tle __ _ Mid -- _ ship -- mite,
  Cheer -- i -- ly my lads yo -- ho!
  
  "" "" “Who -- ’ll go a -- shore to -- _ night,” __ _ _ says he,
  “An’ __ _ spike their _ guns a -- _ long wi’ me?”
  “Why, _ bless ’ee, __ _ sir, come a -- long!” says we,
  
  
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  We __ _ launch’d the cut -- ter and _ shoved her out,
  Cheer -- i -- ly my lads yo -- ho!
  The lub -- bers _ might ha’ _ heard us shout,
  As the Mid -- dy __ _ cried, “Now, my lads, put a -- bout.”
  Cheer -- i -- ly my lads yo -- ho!
  “We _ made _ for the guns, an’ we ram’d _ ’em __ _ tight,
  But the mus -- ket _ shots came _ left and right,
  An’ _ down drops the poor lit -- tle Mid -- ship -- mite,
  
  \unset ignoreMelismata
  Cheer -- i -- ly my lads yo -- ho! __
  Cheer -- i -- ly my lads yo -- ho! __
  With a long, long pull, An’ a strong, strong pull,
  Gai -- ly boys, make her go! __
  An’ we’ll drink to -- night
  To the Mid -- ship -- mite,
  Sing -- ing cheer -- i -- ly, lads, yo -- ho! __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  “I’m __ _ done for "now;" _ good -- _ bye!” says he,
  Stead -- i -- ly, my lads, yo -- ho!
  “You make for the boat, nev -- er mind for me!”
  “We’ll _ take ’ee __ _ back, sir or die,” "" says we,
  Cheer -- i -- ly, my lads, yo -- ho!
  “So we hoist -- ed him __ _ in, in a ter -- ri -- ble __ _ plight,
  An’ we pull’d ev -- ’ry man with _ all his might,
  An’ _ saved the __ _ poor lit -- tle Mid -- ship -- mite,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  \slurDashed
  \tieDashed
  e8~ e |
  e4 e e8~ e e8~ e |
  f4 f e s |
  e8 d c d e4 f |
  e2 s4 e |
  e e8~ e e4 d8~ d |
  
  c4 f e e8[ f] |
  g4 e8( d) c4 c8~ c |
  << {c4. s8} {s4 \teeny c8~ \normalsize c8} >> c4 s |
  d8 d d d fis4 fis |
  
  g2 \teeny s4 g8~ g \normalsize |
  e8~ e f~ f f4 f8~ f |
  f8~ f8~ f8~ f8 f4 f8~ f |
  f4 f8~ f f4 f |
  \tieSolid
  f f f a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 s |
  e8 e e e dis4 dis |
  \slurSolid
  e2( g) |
  g4 g g g |
  
  fis2. g4 |
  \time 3/4
  d4( e f)~ |
  f f4. f8 |
  f2 f4 |
  f8 s8 f4 f |
  e2 e4 |
  e s s |
  e e e |
  e2 e4 |
  
  f2.~ |
  f4 g4. f8 |
  e2 e4 c8 s e4 e |
  d2 d4 |
  b8 s c4 e |
  e e g |
  f2 e4 |
  e2.~ |
  e4 s \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \partial 4
  \slurDashed
  \tieDashed
  c8~ c |
  a4 a g8~ g g8~ g |
  f4 c' c s |
  e,8 d c d e4 c' |
  c2 s4 c |
  a a8~ a gis4 gis8~ gis |
  
  f4 c' c e,8[ f] |
  g4 e8( d) c4 f8~ f |
  << {e4. s8} {s4 \teeny g8~ \normalsize g} >> e4 s |
  g8 g c b a4 c |
  
  b2 \teeny s4 b8~ b \normalsize |
  c8~ c c~ c c4 d8( c) |
  b8~ b~ b~ b8 b4 b8~ b |
  c4 c8~ c c4 c |
  \tieSolid
  b b b a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 s |
  g'8 g g g fis4 a |
  \slurSolid
  g2( bes) |
  b?4 b b d |
  
  c2. b4 |
  \time 3/4
  b4( c d)~ |
  d d4. c8 |
  b2 b4 |
  b8 s d4 b |
  c2 c4 |
  c s s |
  b g b |
  c2 g4 |
  
  b2.~ |
  b4 b4. b8 |
  c2 c4 |
  g8 s g4 c |
  b2 b4 |
  g8 s g4 g |
  g c c |
  b2 g4 |
  g2.~ |
  g4 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  \slurDashed
  \tieDashed
  c,8~ c |
  a'4 a, e'8~ e e8~ e |
  f4 f c d\rest |
  e8 d c d e4 c |
  c2 d4\rest c |
  a' a,8~ a e'4 e8~ e |
  
  f4 f c e8[ f] |
  g4 e8( d) c4 f,8~ f |
  << {c'4. s8} {s4 \teeny c8~ \normalsize c} >> c4 d\rest |
  d8 d d d d4 d |
  
  g2 << d\rest { \teeny s4 g8~ g} >> | \normalsize
  g8~ g g g g4 g8~ g |
  g8~ g~ g~ g8 g4 g8~ g |
  g4 g8~ g g4 g |
  \tieSolid
  g g g a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 d\rest |
  b8 b b b b4 b |
  \slurSolid
  e2( ees) |
  d4 d d d |
  d2. g4 |
  \time 3/4
  g2.~ |
  g4 g4. g8 |
  g2 g4 |
  g8 d\rest g4 g |
  c,2 c4 |
  c d2\rest |
  e4 e e |
  c2 c4 |
  g'2.~ |
  g4 g4.\fermata g8 |
  c,2 c4 |
  c8 d\rest c4 c |
  g'2 g4 |
  g,8 d'\rest g,4 g |
  c c e |
  g2 g4 |
  c,2.~ |
  c4 d\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Midshipmite"}}
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  composer = \markup\oldStyleNum"Stephen Adams (1841–1913)"
  tagline = ""
}}


global = {
  \key e \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  e8[ fis] |
  gis4.( a8) a4 cis |
  \slurDashed b4.( gis8) gis4( b) | \slurSolid
  a4.( gis8) a4 fis |
  gis2 b4\rest e,8[ fis] |
  gis4.( a8) a4 cis |
  
  b2 gis4 b |
  a4.( gis8) a4 fis |
  e2 b'4\rest \tieDashed b8~ b | \tieSolid
  e4.( dis8) cis4. b8 |
  b2 gis4 b |
  a4.( gis8) a4 fis |
  
  gis2 b4\rest \tieDashed b8~ b | \tieSolid
  e4.( dis8) cis4. b8 |
  b2 gis4 b |
  b4 a2 fis4 |
  e2. b'4\rest |
  b1 |
  a2( fis) |
  
  e4 b'\rest fis b\rest |
  gis2 b4\rest b |
  e4.( dis8) cis4. b8 |
  b2 gis4 b |
  a4.( gis8) a4 fis |
  e2 b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	’Mid plea -- sures and \set ignoreMelismata = ##t
  pal -- a -- ces __ _ though _ we may roam,
  Be it ev -- _ er so hum -- ble, there’s no __ _ place like home;
  \unset ignoreMelismata
  A __ charm from the skies seems to hal -- low us there,
  Which, seek through the world, is ne’er met with else -- where.
  
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I __ gaze on the moon \set ignoreMelismata = ##t as I \unset ignoreMelismata tread __ the drear wild,
  And feel that my moth -- er now thinks of her child;
  \set ignoreMelismata = ##t
  As she \unset ignoreMelismata looks on that moon from our own cot -- tage door
  \set ignoreMelismata = ##t
  Through the \unset ignoreMelismata wood -- bine whose fra -- grance shall cheer me no more.
  
  Home, home, home, sweet home,
  There’s no __ place like home,
  Oh there’s no __ place like home.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  An ex -- ile from home, \set ignoreMelismata = ##t splen -- dor \unset ignoreMelismata daz -- zles in vain;
  Oh! give me my low -- ly thatched cot -- tage a -- gain;
  The birds sing -- ing gai -- ly, that came at my call;
  \set ignoreMelismata = ##t
  Give me \unset ignoreMelismata them and that peace of mind, dear -- er than all.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e8[ dis] |
  e4.( fis8) fis4 a |
  \slurDashed gis4.( e8) e4( gis) | \slurSolid
  fis4.( e8) fis4 dis |
  e2 s4 e8[ dis] |
  e4.( fis8) fis4 a |
  
  gis2 e4 gis |
  fis4.( e8) fis4 dis |
  e2 s4 \tieDashed gis8~ gis | \tieSolid
  gis4.( b8) a4. gis8 |
  gis2 e4 gis |
  fis4.( e8) fis4 dis |
  
  e2 s4 \tieDashed gis8~ gis | \tieSolid
  gis4.( b8) a4. gis8 |
  gis2 e4 gis |
  fis4 dis2 dis4 |
  e2. s4 |
  gis1 |
  fis2( dis) |
  
  e4 s dis s |
  e2 s4 gis |
  gis4.( b8) a4. gis8 |
  gis2 e4 gis |
  fis4.( e8) fis4 dis |
  e2 s4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  gis8[ a] |
  b2 b4 e |
  \tieDashed \slurDashed e4.( b8) b4~ b | \slurSolid \tieSolid
  b2 b4 b |
  b2 s4 gis8[ a] |
  b2 b4 e |
  
  e2 b4 b |
  b2 b4 a |
  gis2 s4 \tieDashed b8~ b | \tieSolid
  b2 e4. e8 |
  e2 b4 b |
  b2 b4 b |
  
  b2 s4 \tieDashed b8~ b | \tieSolid
  b2 e4. e8 |
  e2 b4 b |
  dis4 b2 a4 |
  gis2. s4 |
  b1 |
  b |
  
  gis4 s b s |
  b2 s4 b |
  b2 e4. e8 |
  e2 b4 b |
  b2 b4 a |
  gis2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 |
  e2 e4 e |
  \tieDashed e4.~ e8 e4~ e | \tieSolid
  b2 b4 b |
  e2 d4\rest e |
  e2 e4 e |
  
  e2 e4 e |
  b2 b4 b |
  e2 d4\rest \tieDashed e8~ e | \tieSolid
  e2 e4. e8 |
  e2 e4 e |
  b2 b4 b |
  
  e2 d4\rest \tieDashed e8~ e | \tieSolid
  e2 e4. e8 |
  e2 e4 e |
  b4 b2 b4 |
  e2. d4\rest |
  e1 |
  b |
  
  cis4 d\rest b d\rest |
  e2 d4\rest e |
  e2 e4. e8 |
  e2 e4 e |
  b2 b4 b |
  e2 d4\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Home Sweet Home"}}
  poet = \markup\oldStyleNum"John Howard Payne (1791–1852)"
  composer = \markup\oldStyleNum"Sir Henry Rowley Bishop (1786–1855)"
  tagline = ""
}}


