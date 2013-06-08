\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Blue Bells of Scotland"}}
  composer = \markup\oldStyleNum"Dorothea Jordan (1761–1816)"
  tagline = ""
}
\paper {
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
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \key d \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  a'4 |
  d2 cis4 b |
  a2 b4 cis8[ d] |
  fis,4 fis g e |
  d2. \bar""
  
  a'4 |
  d2 cis4 b |
  a2 b4 cis8[ d] |
  fis,4 fis g e |
  d2. \bar""
  
  a'4 |
  fis d fis a |
  d2 b4 cis8[ d] |
  cis4 a b gis |
  a2 \bar""
  
  b4 cis |
  d2 cis4 b |
  a2 b4 cis8[ d] |
  fis,4 fis g e |
  d2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Oh, where! and oh, where! is your High -- land lad -- die gone?
  Oh, where! and oh, where! is your High -- land lad -- die gone?
  He’s gone to fight the foe for King George up -- on the throne;
  And it’s oh! in my heart, how I wish him safe at home!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oh, where! and oh, where! does your High -- land lad -- die dwell?
  Oh, where! and oh, where! does your High -- land lad -- die dwell?
  \set ignoreMelismata = ##t
  He dwelt in mer -- ry Scot -- land at the sign of the Blue Bell;
  \unset ignoreMelismata
  And it’s oh! in my heart, that I love my lad -- die well.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  What clothes, in what clothes is your High -- land lad -- die clad?
  What clothes, in what clothes is your High -- land lad -- die clad?
  His bon -- net’s Sax -- on green, and his waist -- coat of the plaid;
  And it’s oh! in my heart, that I love my High -- land lad.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Sup -- pose, and sup -- pose that your High -- land lad should die?
  Sup -- pose, and sup -- pose that your High -- land lad should die?
  The bag -- pipes shall play o’er him, I’d lay me down and cry;
  And it’s oh! in my heart, that I wish he may not die!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  fis4 |
  fis2 a4 g|
  fis2 g4 d |
  d d cis cis |
  d2.
  
  fis4 |
  fis2 a4 g |
  fis2 g4 d |
  d d cis cis |
  d2.
  
  fis4 |
  d4 d d e |
  d2 e4 e |
  e fis fis e |
  e2
  
  g4 g |
  fis2 a4 g |
  fis2 g4 d |
  d d e cis |
  d2. \bar"|."
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
  a4 |
  a2 b4 cis |
  d2 g,4 a8[ b] |
  a4 a a g |
  fis2.
  
  a4 |
  a2 b4 cis |
  d2 g,4 a8[ b] |
  a4 a a g |
  fis2.
  
  a4 |
  a a a a |
  fis2 gis4 a8[ b] |
  a4 cis d d |
  cis2
  
  b4 a |
  a2 b4 cis |
  d2 d4 a8[ b] |
  a4 a a g |
  fis2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,4 |
  d2 d4 d |
  d2 g4 g |
  a a, a a |
  d2.
  
  d4 |
  d2 d4 d |
  d2 g4 g |
  a a, a a |
  d2.
  
  d4 |
  d fis d cis |
  b2 e4 e |
  a, fis' d e |
  a,2
  
  e'4 e |
  d2 d4 d |
  d2 g4 g |
  a a, a a |
  d2. \bar"|."
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
  \midi {
    \tempo 4 = 120
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}


