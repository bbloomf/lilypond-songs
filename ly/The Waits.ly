\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Waits"}}
  composer = \markup\oldStyleNum"Jeremiah Savile, 1667"
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
  \key g \major
  \time 6/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  g'8 a |
  b2. a |
  g4 e fis g2 a8 b |
  c2. b |
  a4 fis4. g8 a2 b8[ c] |
  
  d4. e8 d[ c] b4 b2 |
  c4 c4. b8 a2 b4 |
  g4 e a fis d g |
  
  %page2
  g g4. fis8 g2 g8 a |
  b2. a |
  g4 e fis g2 a8 b |
  
  c2. b |
  a4 fis4. g8 a2 b8 c |
  d4. e8 d c b4 b2 |
  
  c4 c4. b8 a2 b4 |
  c2 e4 d2 b4 |
  a4 d c^\markup\italic"poco rit." b2 \bar"|."
}
sopWords = \lyricmode {
  %\set stanza = \markup{\dynamic"f  " "1."}
	Let us all sing, mer -- ri -- ly sing,
  let us all sing, mer -- ri -- ly sing,
  
  Till ech -- o a -- round us,
  ech -- o a -- round us,
  ech -- o a -- round us re -- spon -- sive shall ring!
  
  
  
  Fa la la la la la la la,
  Fa la la la la la la la,
  Fa la la la la la la \set associatedVoice = "altos" la,
  la __ la \unset associatedVoice la _ la, 
  \set associatedVoice = "altos"
  Fa la la la la la,
  \unset associatedVoice
  Fa la la la la!
}

sopWordsII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = \markup{\dynamic"pp " "2."}
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
  \partial 4
  b8 c |
  d2. d |
  e4 c a b2 d8 g |
  e2. d |
  d4 d4. d8 d2 g8[ a] |
  
  b4. c8 b[ a] g4 g2 |
  e4 a4. g8 fis2 d4 |
  c4 c e d d b |
  
  %page2
  a4 d c b2 b8 c |
  d2. d |
  e4 c a b2 d8 g |
  
  e2. d |
  d4 d4. d8 d2 g8 g |
  g4. g8 g fis g4 d g~ |
  g e g a2 b4 |
  g e a fis d g |
  g g4. fis8 g2 \bar"|."
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
  \partial 4 
  g8 a |
  b2. d |
  c4 g a g2 a8 g |
  g2. g |
  d'4 a4. g8 fis2 g4 |
  g4. g8 g[ d'] d4 d2 |
  c4 e g, a2 b4 |
  c4 g a d a g |
  a a a g2 g8 a |
  b2. d |
  c4 g a g2 a8 g |
  
  g2. g |
  d'4 a4. g8 fis2 g8 a |
  b4. c8 b a g4 g2 |
  c4 a4. g8 fis4~ fis g |
  c2 c4 a2 d4|
  a a a g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  g8 g |
  g2. fis |
  e4 e d g2 fis8 g |
  c,2. g' |
  fis4 d4. e8 fis2 g4 |

  g4. g8 g[ fis] g4 g2 |
  c,4 a' g d2 g4 |
  e4 c c d fis g |
  
  %page2
  d d d g2 g8 g |
  g2. fis |
  e4 e d g2 fis8 g |
  
  c,2. g' |
  fis4 d4. e8 fis2 g8 a |
  b4. c8 b a g4 g2 |
  
  e4 a4. g8 fis4( d) g |
  e c c d fis g |
  d d d g2 \bar"|."
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


