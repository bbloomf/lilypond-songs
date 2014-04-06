\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Glenlogie"}}
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
       (padding . 1)
       (stretchability . 100))
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
  \key c \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  a'8 a g a a g |
  a a g e4 \bar"" c'16[ d] |
  e8 e d16[ c] c8 a c16[ a] |
  g8 e g a4 \bar"" c16[ d] |

  e8 e d16[ c] d8 d e16[ d] |
  c8 a g16[ e] d4 \bar"" c'16[ a] |
  g8 e c d c d |
  e c' g a4. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Three score o’ no -- bles rade up the King’s ha’
  But bon -- nie Glen -- lo -- gie’s the flow’r o’ them a’
  Wi’ his milk -- white steed and his bon -- nie black e’e,
  “Glen -- lo -- gie, dear mo -- ther, Glen -- lo -- gie for me!”
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  “Haund your tongue, doch -- ter, there’s bet -- ter than he,”
  “O say na sae, mo -- ther, for that can -- na be;
  Tho’ Doum -- lie is great -- er and rich -- er than he,
  Yet if I maun tak’ him, I’ll cer -- tain -- ly dee.”
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  “There is, Glen -- lo -- gie, a let -- ter for thee,
  O there is, Glen -- lo -- gie, a let -- ter for thee!”
  The first line he look’d at, a licht lauch lauched he,
  But ere he had read thro’t tears blind -- ed his e’e.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Then to Glen -- fel -- dy’s but sma’ mirth was there,
  An bon -- nie Jean’s mo -- ther was tear -- in’ her hair,
  “Ye’re wel -- come, Glen -- lo -- gie, ye’re wel -- come,” quo’ she,
  “Ye’re wel -- come, Glen -- lo -- gie, your Jea -- nie to see.”
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Pale and wan was she when_Glen -- lo -- gie gae’d ben,
  But ro -- sy red grew she when e’er he sat down;
  She turned _ a -- wa’ wi’ a smile in her e’e.
  “O din -- na fear, mo -- ther, I’ll may -- be no dee!”
}

altoMusic = \relative c' {
  e8 e e e e e |
  e e e c4 e16[ g] |
  g8 g g16[ e] e8 e f |
  e c e e4 e16[ g] |

  g8 g g16[ e] g8 g c,16[ g'] |
  e8 f e16[ c] a4 c8 |
  c c c a a a |
  c c b c4. \bar"|."
}
tenorMusic = \relative c' {
  c8 c b c c b |
  c c b g4 a16[ d] |
  c8 c g16[ a] a8 c a |
  g g g c4 a16[ d] |
  
  c8 c g16[ a] b8 b g |
  a c c f,4 a8 |
  g g g f a f |
  a a g e4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a8 a e a a e |
  a, a b c4 a16[ b] |
  c8 c b16[ a] a8 a f |
  c' c c a4 a16[ b] |
  
  c8 c b16[ a] g8 g c16[ b] |
  a8 f c' d4 f8 |
  e c e d e d |
  e e e a,4. \bar"|."
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
