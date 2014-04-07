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
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #66
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
#(set-global-staff-size 17.5) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.5 20))) }
global = {
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \mergeDifferentlyDottedOn
}

sopMusic = \relative c' {
	bes'4 g8 bes ees2 |
  c4 ees bes2 |
  bes4 ees,8 f g4 f8 ees |
  g f4. b2\rest |
  bes4 g8 bes ees4. d8 |
  
  c4 ees bes bes8 bes |
  bes4 f8 g aes4. d,8 |
  f8 ees4. b'2\rest |
  c4 ees ees2 |
  d8 d c d ees2 |
  
  c8 d ees c c bes g ees |
  g f4. b2\rest |
  bes8 bes g ees ees'4. d8 |
  c4 ees bes bes8 bes |
  
  bes4 f8 g aes4. d,8 |
  f ees4. b'2\rest \bar"||"\break
  bes4 g8 bes ees2 |
  c4 ees bes2 |
  bes4 ees,8 f g4 f8 ees |
  
  g f4. b2\rest |
  bes4 g8 bes ees4. d8 |
  c4 ees bes bes8 bes |
  bes4 f8 g aes4. d,8 |
  f ees4. b'2\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Dream -- ing of home, dear old home!
  Home of my child -- hood and mo -- ther;
  Oft when I wake, ’tis sweet to find
  I’ve been dream -- ing of home and mo -- ther.
  
  Home, dear home, child -- hood’s hap -- py home!
  When I played with sis -- ter and with bro -- ther;
  ’Twas the sweet -- est joy when we did roam,
  O -- ver hill and through dale with mo -- ther.
  
  

	Dream -- ing of home, dear old home!
  Home of my child -- hood and mo -- ther;
  Oft when I wake, ’tis sweet to find
  I’ve been dream -- ing of home and mo -- ther.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Sleep, balm -- y sleep, close mine eyes,
  Keep me still think -- ing of mo -- ther,
  Hark! ’tis her voice I seem to hear,
  Yes, I’m dream -- ing of home and mo -- ther.
  
  An -- gels come, sooth -- ing me to rest,
  I can feel their pres -- ence and none oth -- er;
  For they sweet -- ly say I shall be blest
  With bright vis -- ions of home and mo -- ther.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Child -- hood has come, come a -- gain,
  Sleep -- ing, I see my dear mo -- ther;
  See her loved form be -- side me kneel,
  While I’m dream -- ing of home and mo -- ther.
  
  Mo -- ther dear, whis -- per to me now,
  Tell me of my sis -- ter and my bro -- ther;
  Now I feel thy hand up -- on my brow,
  Yes I’m dream -- ing of home and mo -- ther.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'4 ees8 g g2 |
  aes4 aes g2 |
  ees4 ees8 ees8 ees4 c8 c |
  ees d4. s2 |
  ees4 ees8 g g4. g8 |
  
  aes4 aes g ees8 ees |
  d4 d8 d8 d4. d8 |
  d ees4. s2 |
  ees4 ees g2 |
  aes8 aes aes aes g2 |
  
  aes8 aes aes aes aes g ees ees |
  ees d4. s2 |
  ees8 ees ees ees g4. g8 |
  aes4 aes g ees8 ees |
  
  d4 d8 d d4. d8 |
  d ees4. s2 \bar"||"
  g4 ees8 g g2 |
  aes4 ees g2 |
  ees4 ees8 d ees4 c8 c |
  
  ees8 d4. s2 |
  ees4 ees8 d ees4. f8 |
  ees4 ees ees ees8 ees8 |
  d4 d8 d d4. bes8 |
  bes bes4. s2 \bar"|."
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
  bes4 bes8 bes8 bes2 |
  c4 c ees( bes) |
  g g8 aes bes4 a8 a |
  bes bes4. s2 |
  g4 bes8 bes bes4. bes8 |
  
  c4 c bes g8 g |
  f4 aes8 g f4. aes8 |
  aes g4. s2 |
  aes4 c bes2 |
  bes8 bes bes bes bes2 |
  
  aes8 bes c ees ees ees bes g |
  bes bes4. s2 |
  g8 g bes g bes4. bes8 |
  c4 c bes g8 g |
  
  f4 aes8 g f4. aes8 |
  aes g4. s2 \bar"||"
  bes4 bes8 bes bes2 |
  c4 c bes2 |
  g4 bes8 bes bes4 a8 a |
  
  bes bes4. s2 |
  g4 bes8 aes g8[ aes bes] b |
  c4 aes g g8 g |
  f4 aes8 g f4. aes8 |
  aes g4. s2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 ees8 ees ees2 |
  ees4 ees ees2 |
  ees4 ees8 ees ees4 f8 f |
  bes,8 bes4. d2\rest |
  ees4 ees8 ees ees4. ees8 |
  
  ees4 ees ees ees8 ees |
  bes4 bes8 bes bes4. bes8 |
  ees ees4. d2\rest |
  aes'4 aes ees2 |
  bes8 bes bes bes ees2 |
  
  aes8 aes aes aes ees ees ees ees |
  bes bes4. d2\rest |
  ees8 ees ees ees ees4. ees8 |
  ees4 ees ees ees8 ees |
  
  bes4 bes8 bes bes4. bes8 |
  ees8 ees4. d2\rest \bar"||"
  ees4 ees8 ees ees2 |
  ees4 ees ees2 |
  ees4 g8 f ees4 f8 f |
  
  bes,8 bes4. d2\rest |
  ees4 ees8 f g4. g,8 |
  aes4 c ees ees8 ees |
  bes4 bes8 bes bes4. bes8 |
  ees ees4. d2\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Dreaming of Home and Mother"}}
  composer = \markup\oldStyleNum"John P. Ordway (1824–1880)"
  tagline = ""
}}


