\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Robin Adair"}}
  composer = \markup\oldStyleNum"Scottish Folk Song"
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
  \key aes \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
}

sopMusic = \relative c' {
	ees4 f g |
  aes4. bes8 c4 |
  ees,16[ aes8.] f16[ aes8.] g16[ bes8.] |
  aes2 b4\rest |
  ees,4 f g |
  aes4. bes8 c4 |
  ees,16[ aes8.] f16[ aes8.] g16[ bes8.] |
  aes2 b4\rest |
  
  c4 c c |
  ees4. ees,8 ees4 |
  c'4 c8[ ees] \times 2/3 {des[ bes g]} |
  aes4. f8 ees4 |
  ees' des8[ c] bes[ aes] |
  aes4. bes8 c4 |
  ees,16[ aes8.] f16[ aes8.] g16[ bes8.] |
  aes2 b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	What’s this dull town to me?
  Rob -- in’s not near.
  What was’t I wished to see,
  What wished to hear?
  
  Where’s all the joy and mirth,
  \set ignoreMelismata = ##t
  That made this town _ a heav’n on earth?
  \unset ignoreMelismata
  Oh! they’re all fled with thee,
  Rob -- in A -- dair.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  What made th’a -- sem -- bly shine?
  Rob -- in A -- dair.
  What made the ball so fine?
  Rob -- in was there.
  
  What, when the play was o’er,
  What made my __ heart so sore?
  Oh! it was part -- ing with
  Rob -- in A -- dair.
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
  c4 des des |
  c4. ees8 ees4 |
  ees4 des des |
  c2 s4 |
  c des des |
  c4. ees8 ees4 |
  ees des des |
  c2 s4 |
  
  ees ees ees |
  ees4. ees8 ees4 |
  ees ees8[ ges] \times 2/3 {f4 ees8} |
  ees4. des8 c4 |
  ees ees ees |
  c4. ees8 ees4 |
  ees des des |
  c2 s4 \bar"|."
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
  aes4 aes ees |
  ees4. g8 aes4 |
  aes aes16[ f8.] ees4 |
  ees2 s4 |
  aes aes ees |
  ees4. g8 aes4 |
  aes aes16[ f8.] ees4 |
  ees2 s4 |
  
  aes aes aes |
  g4. g8 g4 |
  aes4 aes8[ aes] \times 2/3 {aes4 bes8} |
  aes4. aes8 aes4 |
  c4 bes8[ aes] g[ aes] |
  aes4. g8 aes4 |
  aes4 aes16[ f8.] ees4 |
  ees2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,4 des bes |
  aes4. ees'8 aes4 |
  c, des ees |
  aes,2 d4\rest |
  aes des bes |
  aes4. ees'8 aes4 |
  c, des ees |
  aes,2 d4\rest |
  
  aes aes aes |
  ees'4. ees8 ees4 |
  aes4 aes8[ c,] \times 2/3 {des4 des8} |
  c4. des8 aes4 |
  aes aes des8[ c] |
  f4. ees8 aes,4 |
  c des ees |
  aes,2 d4\rest \bar"|."
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


