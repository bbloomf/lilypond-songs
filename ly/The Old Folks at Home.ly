\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Old Folks at Home"}}
  composer = \markup\oldStyleNum"Stephen Foster (1826–1864)"
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
       (stretchability . 60))
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
  first-page-number = #196
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \relative c' {
  \repeat volta 2 {
    g'2 f8 ees g f |
    ees4 ees' c8 ees4. |
    bes2 g4 ees |
    f2. b4\rest |
    g2 f8 ees g f |
    ees4 ees' c8 ees4. |
    
    bes4 g8. ees16 f4 f8.~ f16 |
    ees2. b'4\rest
  }\break
  d4. ees8 f4 bes, |
  bes4. c8 bes4 ees |
  ees c aes c |
  bes2. b4\rest |
  
  
  g2 f8 ees g f |
  ees4 ees' c8 ees4. |
  bes4 g8. ees16 f4 f8. f16 |
  ees2. b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Way down up -- on the Swa -- nee riv -- er,
  Far, far a -- way.
  There’s where my heart is turn -- ing ev -- er
  There’s where the old folks stay.
  
  All the world is sad and drear -- y,
  Ev -- ’ry -- where I roam,
  O dark -- ies, how my heart grows wear -- y,
  Far from the old folks at home.
}

sopWordsII = \lyricmode {
  All up and down the whole cre -- a -- tion,
  Sad -- ly I roam,
  Still long -- ing for the old plan -- ta -- tion,
  \set ignoreMelismata = ##t
  And for the old folks at home.
}
sopWordsIII = \lyricmode {
  \set stanza = #"2. "
  All round the lit -- tle farm I wan -- dered,
  When I was young,
  Then ma -- ny hap -- py days I squan -- dered,
  Ma -- ny the songs I __ sung.
}
sopWordsIV = \lyricmode {
  When I was play -- ing with my broth -- er,
  Hap -- py was I,
  Oh! take me to my kind, old moth -- er,
  There let me live and __ die.
}

sopWordsV = \lyricmode {
  \set stanza = #"3. "
  One lit -- tle hut a -- mong the bush -- es,
  One that I love,
  Still sad -- ly to my mem -- ’ry rush -- es,
  No mat -- ter where I __ rove.
}

sopWordsVI = \lyricmode {
  When will I see the bees a hum -- ming,
  All round the comb?
  When will I hear the ban -- jo tum -- ming
  Down in my good old home.
}

altoMusic = \relative c' {
  \repeat volta 2 {
    ees2 d8 ees ees d |
    ees4 c' aes8 c4. |
    g2 ees4 c |
    d2. s4 |
    ees2 d8 ees ees d |
    ees4 g aes8 c4. |
    
    g4 ees8. ees16 ees4 d8.~ d16 |
    ees2. s4 |
  }
  aes4. aes8 aes4 aes |
  g4. aes8 g4 g |
  c aes aes ees |
  g2. s4 |
  
  
  ees2 d8 ees ees d |
  ees4 g aes8 c4. |
  g4 ees8. ees16 ees4 d8. d16 |
  ees2. s4 |
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
  \repeat volta 2 {
    bes2 aes8 g bes aes |
    g4 aes aes8 aes4. |
    bes2 bes4 a |
    bes2. s4 |
    bes2 aes8 g bes aes |
    g4 bes aes8 aes4. |
    
    bes4 bes8. bes16 bes4 aes8.~ aes16 |
    g2. s4 |
  }
  bes4. bes8 bes4 bes |
  bes4. ees8 bes4 bes |
  aes aes c aes |
  bes2. s4 |
  
  
  bes2 aes8 g bes aes |
  g4 bes aes8 aes4. |
  bes4 bes8. bes16 bes4 aes8. aes16 |
  g2. s4 |
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat volta 2 {
    ees,2 ees8 ees ees ees |
    ees4 ees ees8 ees4. |
    ees2 ees4 f |
    bes,2. d4\rest |
    ees2 ees8 ees ees ees |
    ees4 ees aes,8 aes4. |
    
    bes4 bes8. bes16 bes4 bes8.~ bes16 |
    ees2. d4\rest |
  }
  bes4. bes8 bes4 bes |
  ees4. ees8 ees4 ees |
  aes, aes aes c |
  ees2. d4\rest |
  
  
  ees2 ees8 ees ees ees |
  ees4 ees aes,8 aes4. |
  bes4 bes8. bes16 bes4 bes8. bes16 |
  ees2. d4\rest |
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
}

\score {
  \unfoldRepeats
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
    \tempo 4 = 110
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}


