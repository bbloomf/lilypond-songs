\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Heart Bowed Down"}}
  composer = \markup\oldStyleNum"Michael William Balfe (1808–1870)"
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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #31
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
	\partial 4
  d4 |
  b' b b b |
  \times 2/3 {b8[ a] d,} a'2 d4 |
  d b d8.[ c16] \times2/3{a8[ e fis]} |
  g2 b4\rest \bar"" d,4 |
  
  b'4 b b b |
  \times 2/3 {b8[ a] d,} a'2 d4 |
  d b cis8[ d16 cis]( b8) a |
  a4 ais b cis8[ d] |
  
  d[ fis,] b[ a] \times 2/3{ais8[ b cis,](} fis8.)\fermata e16 |
  d2 b'4\rest d,4 |
  a' a a a |
  \times 2/3 {a8[ g] b,} d2 g4 |
  
  fis4 fis g( c8) b | % TODO put in a turn here.
  a2\fermata b4\rest d, |
  b' b b b |
  \times 2/3 {b8[ a] d,} a'2 d4 |
  d b d( c8) a |
  
  bes2 \bar"" g4( a) |
  b4. b8 c4. a8 |
  dis2( e4)\fermata g,8[ a] |
  b4.. a16 c4.. b16\fermata |
  g2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The heart bowed down by weight of woe,
  To weak -- est hopes will cling,
  To thought and im -- pulse while they flow,
  That can no com -- fort bring, that can, that can no com -- fort bring;
  To those ex -- cit -- ing scenes will blend,
  O’er plea -- sure’s path -- way thrown;
  
  \set associatedVoice = "altos"
  But mem -- ’ry is the on -- ly friend
  That grief can call its own,
  That grief can call its own,
  That grief can call its own.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The mind will in its worst de -- spair
  Still pon -- der o’er the past,
  On mo -- ments of de -- light that were
  Too beau -- ti -- ful to last,
  that were too beau -- ti -- ful __ to last;
  To long de -- part -- ed years ex -- tend,
  Its vis -- ions with them flown;
  
  \set associatedVoice = "altos"
  For mem -- ’ry is the on -- ly friend
  That grief can call its own,
  That grief can call its own,
  That grief can call its own.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 |
  d d d d |
  \times 2/3 {d4 d8} d2 d4 |
  d d d c |
  b2 s4 d |
  
  d d d d |
  \times 2/3 {d4 d8} d2 fis4 |
  g g g4. g8 |
  fis4 e d8[ e] eis4 |
  
  fis8[ d] fis4 \times2/3 {e4 cis8(} cis8.) cis16 |
  d2 s4 d |
  d d d d |
  \times 2/3 {d4 d8} d2 d4 |
  
  d d d( fis8) g |
  fis2 s4 d |
  d d d d |
  \times 2/3 {d4 d8} d2 fis4 |
  g g g( e8) ees |
  
  ees2 cis2 |
  d4. d8 e4. e8 |
  fis2( e4) e |
  d4.. fis16 fis4.. fis16 |
  g2. \bar"|."
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
  b4 |
  b b b b |
  \times 2/3 {c4 c8} c2 c4 |
  b g fis \times2/3{fis8[ g a]} |
  g2 s4 b |
  
  b b b b |
  \times 2/3 {c4 c8} c2 c4 |
  b d e4. e8 |
  d4 cis b b |
  
  a d c( a8.) a16 |
  fis2 s4 fis |
  c' c c c |
  \times 2/3 {b4 b8} b2 b4 |
  
  a a b( a8) d |
  d2 s4 c |
  b b b b |
  \times 2/3 {c4 c8} c2 a4 |
  b d b( c8) c |
  
  bes2 bes |
  b?4. g8 c4. c8 |
  b4( a g) g |
  g4.. d'16 d4.. d16 |
  b2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 |
  g g g g |
  \times 2/3 {fis4 fis8} fis2 d4 |
  g g d d |
  g,2 d'4\rest g |
  
  g g g g |
  \times 2/3 {fis4 fis8} fis2 d4 |
  g g e4. cis8 |
  d4 fis g gis |
  
  a a a,~ a8.\fermata a16 |
  d2 d4\rest d |
  fis fis fis fis |
  \times 2/3 {g4 g8} g2 g4 |
  
  d d d4. d8 |
  d2\fermata d4\rest d |
  g g g g |
  \times 2/3 {fis4 fis8} fis2 d4 |
  g g g4. g8 |
  
  g2 ees |
  d4. d8 c4. c8 |
  b2( c4)\fermata c |
  d4.. d16 d4.. d16\fermata |
  g,2. \bar"|."
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
    \tempo 4 = 90
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


