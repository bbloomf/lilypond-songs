\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"When Johnny Comes Marching Home"}}
  composer = \markup\oldStyleNum"Louis Lambert"
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
  g'8 |
  d g g g4 a8 |
  bes4 a8 bes4 g8 |
  
  f4.~ f4 d8 |
  f4.~ f4 \bar"" g8 |
  d g g g4 a8 |
  bes4 a8 bes4 c8 |
  d4.~ d4 bes8 |
  
  d4.~ d4 \bar"" bes16[ c] |
  d4 d8 d[ c] bes |
  c4 c8 c4 a8 |
  bes4 bes8 bes[ a] g |
  
  a4 a8 a \bar"||" bes c |
  d4. c |
  bes a |
  g8 g g g4 f8 |
  g4.~ g4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	When John -- ny comes march -- ing home a -- gain, Hur -- rah, Hur -- rah,
  We’ll give him a heart -- y wel -- come then, Hur -- rah, Hur -- rah,
  The men will cheer, the boys will shout,
  The la -- dies, they will all turn out,
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The old "" church bell will peal with joy, Hur -- rah, Hur -- rah,
  To wel -- "" come home our dar -- ling boy, Hur -- rah, Hur -- rah,
  The vil -- lage lads and lass -- es say,
  With ro -- ses they will strew the way,
  
  \dropLyricsXV
  And we’ll all feel gay, When John -- ny comes march -- ing home.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Get rea -- "" dy for the Ju -- bi -- lee, Hur -- rah, Hur -- rah,
  We’ll give "" the he -- ro three time three, Hur -- rah, Hur -- rah,
  The laur -- el wreath is rea -- dy now,
  To place up -- on his loy -- al brow,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Let love "" and friend -- ship on that day, Hur -- rah, Hur -- rah,
  Their choic -- "" est trea -- sures then dis -- play, Hur -- rah, Hur -- rah,
  And let each one per -- form some part,
  To fill with joy the war -- rior’s heart,
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8 |
  d d d d4 f8 |
  d4 ees8 d4 d8 |
  d4.~ d4 bes8 |
  d4.~ d4 g8 |
  d d d d4 f8 |
  d4 f8 d4 ees8 |
  f4.~ f4 f8 |
  fis4.~ fis4 g8 |
  f4 f8 f8[ ees] d8 |
  f4 f8 f4 ees8 |
  
  d4 d8 d4 d8 |
  fis4 fis8 fis \bar"||"
  
  g8 f |
  f4. f |
  d fis |
  g8 d d bes4 a8 |
  bes4.~ bes4 \bar"|."
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
  g8 |
  bes bes bes bes4 c8 |
  bes4 c8 bes4 bes8 |
  bes4.~ bes4 f8 |
  bes4.~ bes4 g8 |
  bes bes bes bes4 c8 |
  bes4 c8 bes4 a8 |
  bes4.~ bes4  bes8 |
  a4.~ a4 d8 |
  bes4 bes8 bes4 bes8 |
  a4 a8 a4 c8 |
  
  g4 g8 g[ a] bes8 |
  d4 d8 d \bar"||"
  
  bes8 a |
  bes4. a |
  g d' |
  bes8 bes bes g4 d8 |
  g4.~ g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8 |
  g g g g4 f8 |
  g4 f8 g4 g,8 |
  
  bes4.~ bes4 bes8 |
  bes4.~ bes4 g'8 |
  g g g g4 f8 |
  g4 f8 g4 f8 |
  bes,4.~ bes4 d8 |
  
  d4.~ d4 g,8 |
  bes4 bes8 bes4 bes8 |
  f4 f8 f4 f8 |
  g4 g8 g4 g8 |
  d'4 d8 d \bar"||"
  g f |
  bes,4. f |
  g d' |
  g,8 g g8 g4 d'8 |
  g,4.~ g4 \bar"|."
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
    \tempo 4 = 160
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
