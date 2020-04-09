\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Tailor and the Mouse"}}
  composer = \markup\oldStyleNum"English Folk Song"
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
       (padding . 1)
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
  first-page-number = #104
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  c'4 |
  aes f f f |
  ees c ees2 |
  c4 c8 c c4 c' |
  aes2 f4 \bar"" c' |
  
  aes f f f |
  ees c ees2 |
  c4 c8 c c4 c' |
  aes2 f4 b\rest |\break
  
  aes4 f8 f f4 f |
  aes f f f |
  g bes bes bes |
  g2 bes4 d\rest^\markup\italic"rit." |
  
  aes4^\markup\italic"a tempo" f8 f f4 f |
  ees c8 c ees2 |
  c4 c8 c c4 c' |
  aes2 f4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	There was a tai -- lor had a mouse,
  \repeat unfold 7""
  They lived to -- geth -- er in one house,
  \repeat unfold 7""
  
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The tai -- lor thought the mouse was ill,
  \repeat unfold 7""
  He gave him part of a blue pill,
  \repeat unfold 7""
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The tai -- lor thought his mouse would die,
  Hi did -- dle un -- kum fee -- dle!
  He baked him in an ap -- ple pie,
  Hi did -- dle un -- kum fee -- dle!
  Hi did -- dle un -- kum tar -- um tan -- tum
  Through the town of Ram -- say,
  Hi did -- dle un -- kum o -- ver the lea,
  Hi did -- dle un -- kum fee -- dle!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  The pie was cut, the mouse ran out,
  \repeat unfold 7""
  The tai -- lor fol -- lowed him a -- bout,
  \repeat unfold 7""
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  The tai -- lor found his mouse was dead,
  \repeat unfold 7""
  So_he caught a -- noth -- er in his stead,
}

altoMusic = \relative c' {
  c4 
  c c c des |
  c c c2 |
  c4 c8 c c4 c' |
  aes2 f4 e |
  
  f c c des |
  c c c2 |
  c4 c8 c c4 c' |
  aes2 f4 s |
  
  aes4 f8 f aes[ g] f4 |
  aes f aes8[ g] f4 |
  bes,8[ c] des4 bes8[ c] des4 |
  bes8[ c des c] des[ bes c des] |
  
  c4 c8 c c4 c |
  c4 aes8 aes bes2 |
  c4 c8 c c4 c' |
  aes2 f4 \bar"|."
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
  c4 |
  c aes aes aes |
  g g g2 |
  c,4 c8 c c4 c' |
  aes2 f4 g |
  
  c4 c8[ bes] aes4 aes |
  g g g2 |
  c,4 c8 c c4 c' |
  aes2 f4 s |
  
  c'4 c8 c c4 c |
  c c c c |
  des8[ c] bes[ aes] g4 bes8[ aes] |
  g2 g4 s |
  
  aes4 aes8 aes aes4 aes |
  g aes8 aes g2 |
  c,4 c8 c c4 c' |
  aes2 f4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,4 |
  f aes aes, bes |
  c ees c2 |
  c4 c8 c c4 c' |
  aes2 f4 c |
  
  f c aes bes |
  c ees c2 |
  c4 c8 c c4 c' |
  aes2 f4 d\rest |
  
  f4 aes8 g f4 aes8[ g] |
  f4 aes8[ g] f4 aes8[ g] |
  f4 f f f |
  ees2 ees4 d\rest |
  
  aes f'8 f f4 aes,8[ bes] |
  c4 f8 f ees2 |
  c4 c8 c c4 c' |
  aes2 f4 \bar"|."
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


