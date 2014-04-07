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
       (padding . 2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 70))
  top-system-spacing =
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
  first-page-number = #68
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
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  bes'8 |
  bes4.~ bes4 g8 |
  ees4.~ ees4 g8 |
  bes4. c |
  ees~ ees4 ees8 |
  d4.~ d4 c8 |
  bes4.~ bes4 c8 |
  bes4. f |
  
  bes~ bes4 bes8 |
  bes4.~ bes4 g8 |
  ees4.~ ees4 g8 |
  bes4. c |
  ees~ ees4 ees8 |
  d4.~ d4 a8 |
  bes4.~ bes4 d8 |
  f4.~ f4 c16[ d] |
  
  bes4.~ bes4 bes8 |
  aes4. f' |
  f~ f4 aes,8 |
  g4 f8 g4 c8 |
  bes4.~ bes4 g8 |
  aes4. f' |
  f~ f4 aes,8 |
  g4 f8 g4 c8 |
  bes4.~ bes4 b8\rest |
  
  
  ees4. c8 d ees |
  d4. g,4 g8 |
  c4 c16 c aes8[ bes] c |
  bes4. g4 f8 |
  ees4 f8 g4 f8 |
  ees4 f8 g4 c8 |
  b4.( bes) |
  
  bes ees |
  ees bes |
  c8 b c ees4 c8 |
  bes4.~ bes4 bes8 |
  aes g aes c4 aes8 |
  g4 bes8 ees4 ees8 |
  d[ c] d ees4 c8 |
  f4.~ f4 b,8\rest |
  
  bes4. ees |
  ees bes |
  c8 b c ees4 c8 |
  bes4.~ bes4 bes8 |
  c8 bes c d4 d8 |
  ees4 ees8 f4 f8 |
  g4 ees8 f4 ees8 |
  ees4.~ ees4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Y’heave ho! my lads, the wind blows free,
  A pleas -- ant gale is on our lee;
  And soon a -- cross the o -- cean clear
  Our gal -- lant bark shall brave -- ly steer;
  But ere we part from Eng -- land’s shores to -- night,
  A song we’ll sing for home and beau -- ty bright.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The sail -- or’s life is bold and free,
  His home is on the roll -- ing sea;
  And nev -- er heart more true or brave
  Than his who launch -- es on the wave,
  A -- far he speeds in dis -- tant climes to roam,
  With jo -- cund song he rides the spark -- ling foam.
  
  
  Then here’s to the sail -- or, and here’s to the heart so true,
  Who will think of him up -- on the wa -- ters blue!
  
  Sail -- ing, sail -- ing, o -- ver the bound -- ing main;
  For ma -- ny a storm -- y wind shall blow, ere Jack comes home a -- gain!
  Sail -- ing, sail -- ing, o -- ver the bound -- ing main;
  For ma -- ny a storm -- y wind shall blow, ere Jack comes home a -- gain.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The tide is flow -- ing with the gale,
  Y’heave ho! my lads, set ev -- ’ry sail;
  The har -- bor bar we soon shall clear;
  Fare -- well, once more, to home so dear,
  For when the tem -- pest rag -- es loud and long,
  The home shall be our guid -- ing star and song.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees8 |
  ees4.~ ees4 ees8 |
  ees4.~ ees4 ees8 |
  ees4. ees |
  g~ g4 g8 |
  f4.~ f4 ees8 |
  d4.~ d4 ees8 |
  d4. d |
  
  d~ d4 d8 |
  ees4.~ ees4 ees8 |
  ees4.~ ees4 ees8 |
  ees4. ees |
  g~ g4 g8 |
  f4.~ f4 ees8 |
  d4.~ d4 f8 |
  a4.~ a4 ees8 |
  
  d4.~ d4 d8 |
  f4. aes |
  aes~ aes4 f8 |
  ees4 d8 ees4 ees8 |
  ees4.~ ees4 ees8 |
  f4. aes |
  aes~ aes4 f8 |
  ees4 d8 ees4 ees8 |
  ees4.~ ees4 s8 |
  
  g4. ees8 f g |
  g4. f4 f8 |
  ees4 ees16 ees ees4 ees8 |
  ees4. d4 d8 |
  ees4 f8 g4 f8 |
  ees4 f8 g4 ees8 |
  d4.~ d |
  
  ees g |
  g ees |
  ees8 ees ees ees4 ees8 |
  ees4.~ ees4 ees8 |
  f ees f aes4 f8 |
  ees4 ees8 g4 g8 |
  f4 f8 a4 a8 |
  aes?4.~ aes4 s8 |
  
  ees4. g |
  g ees |
  ees8 ees ees ees4 ees8 |
  ees4.~ ees4 ees8 |
  ees ees ees aes4 aes8 |
  g4 g8 aes4 bes8 |
  bes4 g8 aes4 g8 |
  g4.~ g4 \bar"|."
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
  g8 |
  g4.~ g4 bes8 |
  g4.~ g4 bes8 |
  g4. aes |
  bes~ bes4 bes8 |
  bes4.~ bes4 aes8 |
  aes4.~ aes4 aes8 |
  aes4. aes |
  
  aes~ aes4 aes8 |
  g4.~ g4 bes8 |
  g4.~ g4 bes8 |
  g4. aes |
  bes~ bes4 bes8 |
  bes4.~ bes4 c8 |
  bes4.~ bes4 bes8 |
  c4.~ c4 a8 |
  
  bes4.~ bes4 bes8 |
  d4. d |
  d~ d4 d8 |
  ees4 bes8 bes4 aes8 |
  g4.~ g4 bes8 |
  d4. d |
  d~ d4 bes8 |
  bes4 bes8 bes4 aes8 |
  g4.~ g4 s8 |
  
  c4. g8 g c |
  b4. b4 b8 |
  aes4 aes16 aes aes8[ g] aes |
  g4. bes4 aes8 |
  g4 f8 g4 f8 |
  ees4 f8 g4 g8 |
  g4.( aes) |
  
  g bes |
  bes g |
  aes8 g aes c4 aes8 |
  g4.~ g4 g8 |
  bes bes bes d4 bes8 |
  bes4 g8 bes4 bes8 |
  bes[ a] bes c4 ees8 |
  d4.~ d4 s8 |
  
  g,4. bes |
  bes g |
  aes8 g aes c4 aes8 |
  g4.~ g4 g8 |
  aes8 g aes bes4 bes8 |
  bes4 bes8 bes4 d8 |
  ees4 bes8 bes4 bes8 |
  bes4.~ bes4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees4.~ ees4 ees8 |
  ees4.~ ees4 ees8 |
  ees4. ees |
  ees~ ees4 ees8 |
  bes4.~ bes4 bes8 |
  bes4.~ bes4 bes8 |
  bes4. bes |
  
  bes~ bes4 bes8 |
  ees4.~ ees4 ees8 |
  ees4.~ ees4 ees8 |
  ees4. ees |
  ees~ ees4 ees8 |
  f4.~ f4 f8 |
  f4.~ f4 f8 |
  f4.~ f4 f8 |
  
  bes,4.~ bes4 bes'8 |
  bes4. bes |
  bes~ bes4 bes8 |
  ees,4 ees8 ees4 ees8 |
  ees4.~ ees4 ees8 |
  bes'4. bes |
  bes~ bes4 d,8 |
  ees4 bes8 ees4 ees8 |
  ees4.~ ees4 d8\rest |
  
  c4. c8 c c |
  g'4. g4 g8 |
  aes4 aes16 aes c,8[ bes] aes |
  ees'4. bes4 bes8 |
  ees4 f8 g4 f8 |
  ees4 f8 g4 c,8 |
  g4.( bes) |
  
  ees4. ees |
  ees ees |
  ees8 ees ees ees4 ees8 |
  ees4.~ ees4 ees8 |
  bes bes bes bes4 d8 |
  ees4 ees8 ees4 ees8 |
  f4 f8 f4 f8 |
  bes4.~ bes4 d,8\rest |
  
  ees4. ees |
  ees ees |
  ees8 ees ees ees4 ees8 |
  ees4.~ ees4 ees8 |
  aes, aes aes' f4 f8 |
  ees4 ees8 d4 bes'8 |
  ees,4 ees8 d4 bes8 |
  ees4.~ ees4 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Sailing"}}
  composer = \markup\oldStyleNum"Godfrey Marks (1847–1931)"
  tagline = ""
}}


global = {
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	d'2\p bes4 g |
  f2\< bes\! |
  c bes4 c |
  d\> g, f2\! |
  f\< e4 f\! |
  
  g\mf ees' d2-- |
  c\> d4 c\! |
  f,2. b4\rest |
  c2\p d4 c |
  bes\< g f2\! |
  d'\f ees4-- d |
  
  g,\> d' c2--\! |
  f,\p g4 f\< |
  f ees'\! d2\fermata |
  c2 g4^\markup\italic"rit." a\> |
  bes2. b4\rest\! \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	How can I leave thee!
  How can I from thee part!
  Thou on -- ly hast my heart,
  Sis -- ter, be -- lieve.
  Thou hast this soul of mine
  So close -- ly bound to thine,
  No oth -- er can I love,
  Save thee a -- lone!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Blue is a flow -- ’ret
  Called the “For -- get -- me -- not,”
  Wear it up -- on thy heart,
  And think of me!
  Flow -- ’ret and hope may die,
  Yet love with us shall stay
  That can -- not pass a -- way,
  Sis -- ter, be -- lieve.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Would I a bird were!
  Soon at thy side to be,
  Fal -- con nor hawk would fear,
  Speed -- ing to thee.
  When by the fowl -- er slain,
  I at thy feet should lie,
  Thou sad -- ly shouldst com -- plain,
  Joy -- ful I’d die.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  f2 g4 ees |
  d2 f |
  f f4 f |
  f ees d2 |
  d cis4 d |
  
  ees g f2-- |
  f e4 e |
  f2. s4 |
  ees2 f4 ees |
  d ees d2 |
  f g4-- f |
  
  ees4 f ees2-- |
  ees ees4 ees |
  d g f2 |
  e e4 ees |
  d2. s4 \bar"|."
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
  bes2 bes4 bes |
  bes2 bes |
  a g4 a |
  bes bes bes2 |
  bes bes4 bes |
  
  bes bes bes2-- |
  a2 g4 bes |
  a2. s4 |
  a2 a4 a |
  bes bes bes2 |
  b b4-- b |
  
  c4 b c2-- |
  a2 bes4 a |
  bes bes bes2 |
  g c4 f, |
  f2. s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,2 ees4 ees |
  bes2 d |
  f f4 f |
  bes, bes bes2 |
  bes bes4 bes |
  
  ees ees bes2-- |
  c c4 c |
  f2. d4\rest |
  f2 f4 f |
  bes, ees8[ g] bes4( aes) |
  g2 g4-- g |
  
  c,4 g' c,2-- |
  f2 f4 f |
  bes, bes bes2\fermata |
  c2 c4 f |
  bes,2. d4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"How can I leave thee"}}
  composer = \markup\oldStyleNum"Thuringian Folk Song"
  tagline = ""
}}


