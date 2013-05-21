\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Vive L’Amour"}}
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
       (padding . 0)
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
  \key bes \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8 
  f8 |
  bes bes bes bes a g |
  f g ees d4 bes'8\rest |
  bes bes bes c4 a8 |
  bes4 b8\rest b4\rest \bar""\break
  
  f8 |
  bes bes bes bes a g |
  f g ees d4 b'8\rest |
  bes bes bes c4 a8 bes4 b8\rest b4\rest b8\rest |
  
  d8 d d d d d |
  ees ees ees ees4 b8\rest |
  c c c c c c |
  
  d d d d4 b8\rest |
  bes bes bes bes4 bes8\rest |
  c c c c4 b8\rest |
  a a a a4 a8 |
  bes4.~ bes4 \bar"|."
}
sopWords = \lyricmode {
	\set stanza = #"1."
  Let ev -- ’ry good fel -- low now fill up his glass,
    \repeat unfold 6 \skip1
    
  And drink to the health of our glo -- ri -- ous class,
}

sopWordsII = \lyricmode {
  \set stanza = #"2."
  Now let ev -- ’ry mar -- ried man drink to his wife.
    \markup\italic Vi -- ve \markup\italic la \markup\italic com -- \markup\italic pag -- \markup\italic nie,
  The joy of his bo -- som and plague of his life.
    \markup\italic Vi -- \markup\italic ve \markup\italic la \markup\italic com -- \markup\italic pag -- \markup\italic nie.
    
  \set stanza = \markup\dynamic"ff "
  \markup\italic Vi -- \markup\italic ve \markup\italic la, \markup\italic vi -- \markup\italic ve \markup\italic la, \markup\italic vi -- \markup\italic ve \markup\italic l’a -- \markup\italic mour,
  \markup\italic Vi -- \markup\italic ve \markup\italic la, \markup\italic vi -- \markup\italic ve \markup\italic la, \markup\italic vi -- \markup\italic ve \markup\italic l’a -- \markup\italic mour,
  \markup\italic vi -- \markup\italic ve \markup\italic l’a -- \markup\italic mour,
  \markup\italic vi -- \markup\italic ve \markup\italic l’a -- \markup\italic mour,
  \markup\italic vi -- \markup\italic ve \markup\italic la \markup\italic com -- \markup\italic pag -- \markup\italic nie!
}
sopWordsIII = \lyricmode {
  \set stanza = #"3."
  Come fill up your glass -- es, I’ll give you a toast
  \repeat unfold 6 \skip1
  A health to our dear friend, our kind wor -- thy host.
}
sopWordsIV = \lyricmode {
  \set stanza = #"4."
}
sopWordsV = \lyricmode {
  \set stanza = #"5."
}

altoMusic = \relative c' {
  \partial 8 f8 |
  d d d g f ees |
  d ees c bes4 s8 |
  d8 d d f4 ees8 |
  d4 s4. \bar""
  
  f8 |
  d d d g f ees |
  d ees c bes4 s8 |
  d8 d d f4 ees8 |
  d4 s2 |
  
  f8 f f f f f |
  g g g g4 s8 |
  a a a a a a |
  
  bes bes bes bes4 s8 |
  d, d d d4 s8 |
  ees ees ees ees4 s8 |
  f f f f4 ees8 |
  d4.~ d4 \bar"|."
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
  \partial 8
  f,8
  f f f bes c bes |
  bes bes a bes4 s8 |
  bes8 bes bes a4 c8 |
  bes4 s4. \bar""
  
  f8
  f f f bes c bes |
  bes bes a bes4 s8 |
  bes8 bes bes a4 c8 |
  bes4 s2 |
  
  bes8 bes bes bes bes bes |
  bes bes bes bes4 s8 |
  f f f f f f |
  
  f f f f4 s8 |
  bes bes bes bes4 s8 |
  g g g g4 s8 |
  c c c c4 c8 |
  bes4.~ bes4 \bar"|."
}
tenorWords = \lyricmode {
	
}

bassMusic = \relative c' {
  \partial 8
  f,8 |
  bes, bes bes ees ees ees |
  f ees f bes,4 d8\rest |
  g8 g f f4 f8 |
  bes,4 d8\rest d4\rest \bar""
  
  f8 |
  bes, bes bes ees ees ees |
  f ees f bes,4 d8\rest |
  g8 g f f4 f8 |
  bes,4 d8\rest d4\rest d8\rest |
  
  bes8 bes bes bes bes bes |
  ees ees ees ees4 d8\rest |
  f f f f f f |
  bes, bes bes bes4 d8\rest |
  g g g g4 d8\rest |
  c c c c4 d8\rest |
  f f f f4 f8 |
  bes,4.~ bes4 \bar "|."
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
    \new Lyrics \lyricsto "sopranos" \sopWords
    \new Lyrics \lyricsto "sopranos" \sopWordsII
    \new Lyrics \lyricsto "sopranos" \sopWordsIII
    \new Lyrics \lyricsto "sopranos" \sopWordsIV
    \new Lyrics \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
  >>
  >>
    %\new PianoStaff << \new Staff { \new Voice { \global \pianoRH } } \new Staff { \clef "bass" \global \pianoLH } >>
  \midi {
    \tempo 4 = 180
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


