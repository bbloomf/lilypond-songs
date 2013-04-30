\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"America"}}
  poet = \markup\oldStyleNum"Samuel Francis Smith (1808–1895)"
  composer = \markup\oldStyleNum"Traditional"
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
  outer-margin = 1\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  %first-page-number = #196
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
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \transpose bes g {
  \relative c' {
    bes'4 bes c |
    a4. bes8 c4 |
    d d ees |
    d4. c8 bes4 |
    c bes a |
    
    bes2. |
    f'4 f f |
    f4. ees8 d4 |
    ees ees ees |
    ees4. d8 c4 |
    d ees8[ d] c[ bes] |
    
    %page2
    d4. ees8 f4 |
    g8[^\markup\italic"rit." ees] d4 c |
    bes2.\bar"|."
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  My coun -- try, ’tis of thee,
  Sweet land of lib -- er -- ty,
  Of thee I sing;
  
  Land where my fa -- thers died,
  Land of the pil -- grims’ pride,
  From ev -- ’ry moun -- tain -- side
  Let free -- dom ring!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  My na -- tive coun -- try, thee,
  Land of the no -- ble free,
  Thy name I love;
  
  I love thy rocks and rills,
  Thy woods and tem -- pled hills;
  My hearts with rap -- ture thrills,
  Like that a -- bove.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Let mu -- sic swell the breeze,
  And ring from all the trees
  Sweet free -- dom’s song;
  
  Let mor -- tal tongues a -- wake;
  Let all that breathe par -- take;
  Let rocks their si -- lence break,
  The sound pro -- long.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Our fa -- thers’ God to Thee,
  Au -- thor of lib -- er -- ty,
  To Thee we sing.
  
  Long may our land be bright,
  With free -- dom’s ho -- ly light,
  Pro -- tect us by Thy might,
  Great God our King.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 e e |
  d4. e8 fis4 |
  g g g |
  g4. fis8 e4 |
  
  e4 d d |
  d2. |
  b'4 g b |
  b4. a8 g4 |
  fis a fis |
  
  a4. g8 fis4 |
  g4 fis8[ g] d8[ e] |
  g4. fis8 g4 |
  g4 g fis |
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
  b4 b c |
  a4. a8 d4 |
  d e e |
  d4. c8 b4 |
  
  c4 b a |
  b2. |
  d4 d d |
  d4. fis8 d4 |
  d d d |
  
  d4. d8 d4 |
  d d4 c |
  d4. d8 b4 |
  c8[ e] d4 c |
  b2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 e c |
  d4. d8 d4 |
  g e c |
  d4. dis8 e4 |
  
  c d d |
  g,2. |
  g4 b d |
  g4. d8 g4 |
  d fis a |
  
  d,4. g8 d4 |
  g a8[ g] fis[ e] |
  d8[ c] b[ a] g4 |
  c4 d d |
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
    \tempo 4 = 95
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


