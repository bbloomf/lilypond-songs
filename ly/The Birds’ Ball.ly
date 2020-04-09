\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Birds’ Ball"}}
  composer = \markup\oldStyleNum"Septimus Winner (1827–1902)"
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
  first-page-number = #85
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
  \key ees \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed \tieDashed
}

sopMusic = \relative c' {
	\partial 16
  \teeny bes'16 |
  \normalsize
  ees8 bes g bes16~ bes |
  c8 bes16~ bes g8~ g16 g |
  f8 g aes f16~ f |
  g8 c bes8~ bes16~ bes16 |
  
  ees8 bes g bes16~ bes |
  c8 bes g8~ g16 g16 |
  f8 g16~ g aes8 f16~ f |
  ees8 ees ees4 |
  
  \repeat volta 2 {
    c'8 c c16 d ees8 |
    bes bes bes16 aes g8 |
    f g aes f |
    g16 aes bes c bes4 |
    
    c8 c c16 d ees8 |
    bes bes bes16 aes g8 |
    f16 g aes bes c8 d |
    ees ees ees4
  }
  
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	"" Spring once said to the night -- in -- _ gale, __ _
  I mean to give you __ _ birds a ball; __ _ _
  Pray, ma’am ask the __ _ bird -- ies all, __ _
  The birds and __ _ bird -- ies, __ _ great and small.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  "" Soon they came from the bush and _ tree, __ _
  "" Sing -- ing sweet their _ songs of glee: __ _ _
  Each one fresh from its co -- zy nest, __ _ ""
  Each one __ _ dressed in its Sun -- day best.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  The Wren and Cuck -- oo __ _ danced for _ life, __ _
  The ra -- ven waltzed with the yellow -- bird’s wife,
  The __ _ awk -- ward owl and the bash -- ful jay, __ _ ""
  Wished each _ other a __ _ “very good day.”
  
  Tra la la la la,
  Tra la la la la,
  Tra la la la,
  Tra la la la la,
  
  Tra la la la la,
  Tra la la la la,
  Tra la la la la la,
  Tra la la.
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  A Wood -- pecker  came from his hole in the tree, __ _
  And brought his bill to the com -- pa -- ny,
  For the cher -- ries ripe and the ber -- ries red;
  ’Twas a very long __ _ bill so the bird -- ies said.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  They danced all day till the sun was _ low,
  ’Till the moth -- er birds pre -- _ pared to go,
  When _ one and all both _ great and small, __ _
  Flew home to their nests from the bird -- ies’ ball.
}

altoMusic = \relative c' {
  \teeny g'16 |
  \normalsize g8 g g g16~ g |
  aes8 g16~ g ees8~ ees16 ees16 |
  d8 ees f d16~ d |
  ees8 aes g8~ g16~ g |
  
  g8 g ees g16~ g |
  aes8 g ees8~ ees16 ees16 |
  d8 ees16~ ees f8 d16~ d |
  bes8 bes ees4 |
  
  ees8 ees ees16 ees ees8 |
  ees ees ees16 ees ees8 |
  d8 ees f d |
  ees16 f g aes g4 |
  
  ees8 ees ees16 ees ees8 |
  ees8 ees ees16 ees ees8 |
  d16 ees f g aes8 aes |
  g8 g g4 
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
  \teeny bes16 |
  \normalsize bes8 bes bes ees16~ ees |
  ees8 ees16~ ees bes8~ bes16 bes16 |
  bes8 bes bes bes16~ bes |
  bes8 ees ees8~ ees16~ ees |
  
  bes8 bes bes ees16~ ees |
  ees8 ees bes8~ bes16 bes16 |
  bes8 bes16~ bes bes8 aes16~ aes |
  g8 g g4 |
  
  aes8 aes aes16 aes aes8 |
  g8 g g16 aes bes8 |
  bes bes bes bes |
  bes16 bes bes bes bes4 |
  
  aes8 aes aes16 bes c8 |
  g8 g g16 aes bes8 |
  bes16 bes bes bes bes8 bes |
  bes8 bes bes4 
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \teeny ees,16 |
  \normalsize ees8 ees ees ees16~ ees |
  ees8 ees16~ ees ees8~ ees16 ees16 |
  bes8 bes bes bes16~ bes |
  ees8 ees ees8~ ees16~ ees |
  
  ees8 ees ees ees16~ ees |
  ees8 ees ees8~ ees16 ees16 |
  bes8 bes16~ bes bes8 bes16~ bes |
  ees8 ees ees4 |
  
  aes,8 aes aes16 bes c[ d] |
  ees8 ees ees16 ees ees8 |
  bes8 bes bes bes |
  ees16 ees ees ees ees4 |
  
  aes8 aes aes16 aes aes8 |
  ees8 ees ees16 ees ees8 |
  bes16 bes bes bes bes8 bes |
  ees8 ees ees4 
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
    \tempo 4 = 130
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


