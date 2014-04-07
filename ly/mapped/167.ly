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
       (stretchability . 80))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #167
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
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	b'4 b c d |
  d c b a |
  g g a b |
  b4. a8 a2 |
  
  b4 b c d |
  d c b a |
  g g a b |
  a4. g8 g2 |
  
  a4 a b g |
  a b8[ c] b4 g |
  a b8[ c] b4 a |
  g a d, b'~ |
  
  b b c d |
  d c b a |
  g^\markup\italic"rall." g a b |
  a4. g8 g2\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Hail to Joy, from heav’n de -- scend -- ing;
  Hail Joy, all ye here be -- low.
  At her shrine we now are bend -- ing;
  Let the world our glad -- ness know.
  
  Though by cus -- tom’s law di -- vid -- ed,
  Now we meet on com -- mon ground.
  We __ are broth -- ers, all u -- nit -- ed
  When joy in our hearts is found.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  We, with whom kind for -- tune fa -- vors
  Lov -- ing friend in -- stead of foe,
  We should be for -- e’er re -- joic -- ing,
  For through him we heav -- en know.
  
  They who scorn the pledge of friend -- ship
  On -- ly for them -- selves do live,
  They __ are doomed to walk for -- got -- ten
  Who re -- fuse their hearts to give.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Hail to Joy, from heav’n de -- scend -- ing;
  Bring -- ing heav’n on earth to you!
  Broth -- ers, in yon might -- y spac -- es
  Dwells our God whose love is true.
  
  O ye mil -- lions, bow be -- fore Him;
  Seek Him, He is ev -- er nigh!
  We __ are broth -- ers, all u -- nit -- ed,
  Fa -- ther’d by one God on high.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 d d d |
  d d d d8[ c] |
  b4 d d d |
  d4. d8 d2 |
  
  d4 d e f |
  f e d c |
  b b c b |
  c4. b8 b2 |
  
  d4 d d d |
  d d d d |
  d b8[ a] fis'4 fis |
  b, cis d d~ |
  
  d d e f |
  f e d c |
  b b c d |
  c4. b8 b2 \bar"|."
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
  g4 g a b |
  b a g g8[ a] |
  b4 b fis g |
  g4. fis8 fis2 |
  
  g4 g g g |
  g g g g |
  g g fis g |
  fis4. g8 g2 |
  
  fis4 fis g b |
  fis g8[ a] g4 b |
  fis g8[ a] fis4 dis |
  e e fis g~ |
  
  g g g a8[ b] |
  c4 c g e |
  d g g g |
  fis4. g8 g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 g g g |
  d d d e8[ fis] |
  g4 d d d |
  d4. d8 d2 |
  
  g4 g g g |
  c, c c c |
  d d d d |
  d4. d8 g,2 |
  
  d'4 d d d |
  d d d d |
  d d dis b |
  e a, d? d( |
  
  g) g g g |
  c, c c c |
  d d d d |
  d4. d8 g,2 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Ode to Joy"}}
  composer = \markup\oldStyleNum"Ludwig van Beethoven (1770–1827)"
  poet = \markup\oldStyleNum"Friedrich von Schiller (1759–1805)"
  tagline = ""
}}


